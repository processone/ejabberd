%%%----------------------------------------------------------------------
%%% File    : ejabberd_web_admin.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Administration web interface
%%% Created :  9 Apr 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-export([process/2, list_users/4,
	 list_users_in_diapason/4, pretty_print_xml/1,
	 term_to_id/1]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

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
get_acl_rule([<<"logo-fill.png">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"favicon.ico">>], _) ->
    {<<"localhost">>, [all]};
get_acl_rule([<<"additions.js">>], _) ->
    {<<"localhost">>, [all]};
%% This page only displays vhosts that the user is admin:
get_acl_rule([<<"vhosts">>], _) ->
    {<<"localhost">>, [all]};
%% The pages of a vhost are only accessible if the user is admin of that vhost:
get_acl_rule([<<"server">>, VHost | _RPath], Method)
    when Method =:= 'GET' orelse Method =:= 'HEAD' ->
    {VHost, [configure, webadmin_view]};
get_acl_rule([<<"server">>, VHost | _RPath], 'POST') ->
    {VHost, [configure]};
%% Default rule: only global admins can access any other random page
get_acl_rule(_RPath, Method)
    when Method =:= 'GET' orelse Method =:= 'HEAD' ->
    {global, [configure, webadmin_view]};
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
    {_Base, _, Items} = make_host_menu(Host, [], Lang, JID, Level),
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
		 ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					       ?T("Unauthorized"))])};
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
		 ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					       ?T("Unauthorized"))])}
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
	     ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					   ?T("Unauthorized"))])};
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
	     ejabberd_web:make_xhtml([?XCT(<<"h1">>,
					   ?T("Unauthorized"))])}
    end.

get_auth_admin(Auth, HostHTTP, RPath, Method) ->
    case Auth of
      {SJID, Pass} ->
	  {HostOfRule, AccessRule} = get_acl_rule(RPath, Method),
	    try jid:decode(SJID) of
		#jid{user = <<"">>, server = User} ->
		    case ejabberd_router:is_my_host(HostHTTP) of
			true ->
			    get_auth_account(HostOfRule, AccessRule, User, HostHTTP,
					     Pass);
			_ ->
			    {unauthorized, <<"missing-server">>}
		    end;
		#jid{user = User, server = Server} ->
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

make_xhtml(Els, Host, Lang, JID, Level) ->
    make_xhtml(Els, Host, cluster, Lang, JID, Level).

-spec make_xhtml([xmlel()],
                 Host::global | binary(),
                 Node::cluster | atom(),
                 Lang::binary(),
                 jid(),
                 Level::integer()) ->
    {200, [html], xmlel()}.
make_xhtml(Els, Host, Node, Lang, JID, Level) ->
    Base = get_base_path_sum(0, 0, Level),
    MenuItems = make_navigation(Host, Node, Lang, JID, Level),
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
				    children = []}]},
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
				   ?C(<<" (c) 2002-2024 ">>),
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

logo_fill() ->
    case misc:read_img("admin-logo-fill.png") of
	{ok, Img} -> Img;
	{error, _} -> <<>>
    end.

%%%==================================
%%%% process_admin

process_admin(global, #request{path = [], lang = Lang}, AJID) ->
    MenuItems = get_menu_items(global, cluster, Lang, AJID, 0),
    Disclaimer = maybe_disclaimer_not_admin(MenuItems, AJID, Lang),
    make_xhtml((?H1GL((translate:translate(Lang, ?T("Administration"))), <<"">>,
		      <<"Contents">>))
		 ++ Disclaimer ++
		 [?XE(<<"ul">>,
		      [?LI([?ACT(MIU, MIN)])
		       || {MIU, MIN}
			      <- MenuItems])],
	       global, Lang, AJID, 0);
process_admin(Host, #request{path = [], lang = Lang}, AJID) ->
    make_xhtml([?XCT(<<"h1">>, ?T("Administration")),
		?XE(<<"ul">>,
		    [?LI([?ACT(MIU, MIN)])
		     || {MIU, MIN}
			    <- get_menu_items(Host, cluster, Lang, AJID, 2)])],
	       Host, Lang, AJID, 2);
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
process_admin(_Host, #request{path = [<<"logo-fill.png">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"image/png">>}, last_modified(),
      cache_control_public()],
     logo_fill()};
process_admin(_Host, #request{path = [<<"additions.js">>]}, _) ->
    {200,
     [{<<"Content-Type">>, <<"text/javascript">>},
      last_modified(), cache_control_public()],
     additions_js()};
process_admin(global, #request{path = [<<"vhosts">>], lang = Lang}, AJID) ->
    Res = list_vhosts(Lang, AJID),
    make_xhtml((?H1GL((translate:translate(Lang, ?T("Virtual Hosts"))),
		      <<"basic/#xmpp-domains">>, ?T("XMPP Domains")))
		 ++ Res,
	       global, Lang, AJID, 1);
process_admin(Host,  #request{path = [<<"users">>], q = Query,
			      lang = Lang}, AJID)
    when is_binary(Host) ->
    Res = list_users(Host, Query, Lang, fun url_func/1),
    make_xhtml([?XCT(<<"h1">>, ?T("Users"))] ++ Res, Host,
	       Lang, AJID, 3);
process_admin(Host, #request{path = [<<"users">>, Diap],
			     lang = Lang}, AJID)
    when is_binary(Host) ->
    Res = list_users_in_diapason(Host, Diap, Lang,
				 fun url_func/1),
    make_xhtml([?XCT(<<"h1">>, ?T("Users"))] ++ Res, Host,
	       Lang, AJID, 4);
process_admin(Host, #request{path = [<<"online-users">>],
			     lang = Lang}, AJID)
    when is_binary(Host) ->
    Res = list_online_users(Host, Lang),
    make_xhtml([?XCT(<<"h1">>, ?T("Online Users"))] ++ Res,
	       Host, Lang, AJID, 3);
process_admin(Host, #request{path = [<<"last-activity">>],
			     q = Query, lang = Lang}, AJID)
    when is_binary(Host) ->
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
    PageH1 = ?H1GL(translate:translate(Lang, ?T("Users Last Activity")), <<"modules/#mod-last">>, <<"mod_last">>),
    make_xhtml(PageH1 ++
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
		   ++ Res,
	       Host, Lang, AJID, 3);
process_admin(Host, #request{path = [<<"stats">>], lang = Lang}, AJID) ->
    Res = get_stats(Host, Lang),
    PageH1 = ?H1GL(translate:translate(Lang, ?T("Statistics")), <<"modules/#mod-stats">>, <<"mod_stats">>),
    Level = case Host of
	global -> 1;
	_ -> 3
    end,
    make_xhtml(PageH1 ++ Res, Host, Lang, AJID, Level);
process_admin(Host, #request{path = [<<"user">>, U],
			     q = Query, lang = Lang}, AJID) ->
    case ejabberd_auth:user_exists(U, Host) of
      true ->
	  Res = user_info(U, Host, Query, Lang),
	  make_xhtml(Res, Host, Lang, AJID, 4);
      false ->
	  make_xhtml([?XCT(<<"h1">>, ?T("Not Found"))], Host,
		     Lang, AJID, 4)
    end;
process_admin(Host, #request{path = [<<"nodes">>], lang = Lang}, AJID) ->
    Res = get_nodes(Lang),
    Level = case Host of
	global -> 1;
	_ -> 3
    end,
    make_xhtml(Res, Host, Lang, AJID, Level);
process_admin(Host, #request{path = [<<"node">>, SNode | NPath],
			     q = Query, lang = Lang}, AJID) ->
    case search_running_node(SNode) of
      false ->
	  make_xhtml([?XCT(<<"h1">>, ?T("Node not found"))], Host,
		     Lang, AJID, 2);
      Node ->
	  Res = get_node(Host, Node, NPath, Query, Lang),
	  Level = case Host of
		global -> 2 + length(NPath);
		_ -> 4 + length(NPath)
	  end,
	  make_xhtml(Res, Host, Node, Lang, AJID, Level)
    end;
%%%==================================
%%%% process_admin default case
process_admin(Host, #request{lang = Lang} = Request, AJID) ->
    Res = case Host of
	      global ->
		  ejabberd_hooks:run_fold(
		    webadmin_page_main, Host, [], [Request]);
	      _ ->
		  ejabberd_hooks:run_fold(
		    webadmin_page_host, Host, [], [Host, Request])
	  end,
    Level = case Host of
	global -> length(Request#request.path);
	_ -> 2 + length(Request#request.path)
    end,
    case Res of
      [] ->
	  setelement(1,
		     make_xhtml([?XC(<<"h1">>, <<"Not Found">>)], Host, Lang,
				AJID, Level),
		     404);
      _ -> make_xhtml(Res, Host, Lang, AJID, Level)
    end.

term_to_id(T) -> base64:encode((term_to_binary(T))).

%%%==================================
%%%% list_vhosts

list_vhosts(Lang, JID) ->
    list_vhosts2(Lang, list_vhosts_allowed(JID)).

list_vhosts_allowed(JID) ->
    Hosts = ejabberd_option:hosts(),
    lists:filter(fun (Host) ->
					any_rules_allowed(Host,
						     [configure, webadmin_view],
						     JID)
				end,
				Hosts).

list_vhosts2(Lang, Hosts) ->
    SHosts = lists:sort(Hosts),
    [?XE(<<"table">>,
	 [?XE(<<"thead">>,
	      [?XE(<<"tr">>,
		   [?XCT(<<"td">>, ?T("Host")),
		    ?XACT(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          ?T("Registered Users")),
		    ?XACT(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          ?T("Online Users"))])]),
	  ?XE(<<"tbody">>,
	      (lists:map(fun (Host) ->
				 OnlineUsers =
				     length(ejabberd_sm:get_vh_session_list(Host)),
				 RegisteredUsers =
				     ejabberd_auth:count_users(Host),
				 ?XE(<<"tr">>,
				     [?XE(<<"td">>,
					  [?AC(<<"../server/", Host/binary,
						 "/">>,
					       Host)]),
				      ?XAE(<<"td">>,
                                           [{<<"class">>, <<"alignright">>}],
                                           [?AC(<<"../server/", Host/binary, "/users/">>,
                                                pretty_string_int(RegisteredUsers))]),
				      ?XAE(<<"td">>,
                                           [{<<"class">>, <<"alignright">>}],
                                           [?AC(<<"../server/", Host/binary, "/online-users/">>,
                                                pretty_string_int(OnlineUsers))])])
			 end,
			 SHosts)))])].

maybe_disclaimer_not_admin(MenuItems, AJID, Lang) ->
    case {MenuItems, list_vhosts_allowed(AJID)} of
        {[_], []} ->
            [?XREST(?T("Apparently your account has no administration rights in this server. "
                       "Please check how to grant admin rights in: "
                       "https://docs.ejabberd.im/admin/installation/#administration-account"))
            ];
        _ ->
            []
    end.

%%%==================================
%%%% list_users

list_users(Host, Query, Lang, URLFunc) ->
    Res = list_users_parse_query(Query, Host),
    Users = ejabberd_auth:get_users(Host),
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
      ok -> [?XREST(?T("Submitted"))];
      error -> [?XREST(?T("Bad format"))];
      nothing -> []
    end
      ++
      [?XAE(<<"form">>,
	    [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	    ([?XE(<<"table">>,
		  [?XE(<<"tr">>,
		       [?XC(<<"td">>, <<(translate:translate(Lang, ?T("User")))/binary, ":">>),
			?XE(<<"td">>,
			    [?INPUT(<<"text">>, <<"newusername">>, <<"">>)]),
			?XE(<<"td">>, [?C(<<" @ ", Host/binary>>)])]),
		   ?XE(<<"tr">>,
		       [?XC(<<"td">>, <<(translate:translate(Lang, ?T("Password")))/binary, ":">>),
			?XE(<<"td">>,
			    [?INPUT(<<"password">>, <<"newuserpassword">>,
				    <<"">>)]),
			?X(<<"td">>)]),
		   ?XE(<<"tr">>,
		       [?X(<<"td">>),
			?XAE(<<"td">>, [{<<"class">>, <<"alignright">>}],
			     [?INPUTT(<<"submit">>, <<"addnewuser">>,
				      ?T("Add User"))]),
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
	  try jid:decode(<<Username/binary, "@",
				    Host/binary>>)
	      of
	    #jid{user = User, server = Server} ->
		case ejabberd_auth:try_register(User, Server, Password)
		    of
		  {error, _Reason} -> error;
		  _ -> ok
		end
	  catch _:{bad_jid, _} ->
		  error
	  end;
      false -> nothing
    end.

list_users_in_diapason(Host, Diap, Lang, URLFunc) ->
    Users = ejabberd_auth:get_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    [S1, S2] = ejabberd_regexp:split(Diap, <<"-">>),
    N1 = binary_to_integer(S1),
    N2 = binary_to_integer(S2),
    Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
    [list_given_users(Host, Sub, <<"../../">>, Lang,
		      URLFunc)].

list_given_users(Host, Users, Prefix, Lang, URLFunc) ->
    ModOffline = get_offlinemsg_module(Host),
    ?XE(<<"table">>,
	[?XE(<<"thead">>,
	     [?XE(<<"tr">>,
		  [?XCT(<<"td">>, ?T("User")),
		   ?XACT(<<"td">>,
                         [{<<"class">>, <<"alignright">>}],
                         ?T("Offline Messages")),
		   ?XCT(<<"td">>, ?T("Last Activity"))])]),
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
					      case get_last_info(User, Server) of
						not_found -> translate:translate(Lang, ?T("Never"));
						{ok, Shift, _Status} ->
						    TimeStamp = {Shift div
								   1000000,
								 Shift rem
								   1000000,
								 0},
						    {{Year, Month, Day},
						     {Hour, Minute, Second}} =
							calendar:now_to_local_time(TimeStamp),
						    (str:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
										   [Year,
										    Month,
										    Day,
										    Hour,
										    Minute,
										    Second]))
					      end;
					  _ -> translate:translate(Lang, ?T("Online"))
					end,
				?XE(<<"tr">>,
				    [?XE(<<"td">>,
					 [?AC((URLFunc({user, Prefix,
							misc:url_encode(User),
							Server})),
					      (us_to_list(US)))]),
				     ?XAE(<<"td">>,
                                          [{<<"class">>, <<"alignright">>}],
                                          FQueueLen),
				     ?XC(<<"td">>, FLast)])
			end,
			Users)))]).

get_offlinemsg_length(ModOffline, User, Server) ->
    case ModOffline of
      none -> <<"disabled">>;
      _ ->
	  pretty_string_int(ModOffline:count_offline_messages(User,Server))
    end.

get_offlinemsg_module(Server) ->
    case gen_mod:is_loaded(Server, mod_offline) of
      true -> mod_offline;
      false -> none
    end.

get_lastactivity_menuitem_list(Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
	true ->
	    case mod_last_opt:db_type(Server) of
		mnesia -> [{<<"last-activity">>, ?T("Last Activity")}];
		_ -> []
	    end;
	false ->
	    []
    end.

get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
	true ->
	    mod_last:get_last_info(User, Server);
	false ->
	    not_found
    end.

us_to_list({User, Server}) ->
    jid:encode({User, Server, <<"">>}).

su_to_list({Server, User}) ->
    jid:encode({User, Server, <<"">>}).

%%%==================================
%%%% get_stats

get_stats(global, Lang) ->
    OnlineUsers = ejabberd_sm:connected_users_number(),
    RegisteredUsers = lists:foldl(fun (Host, Total) ->
					  ejabberd_auth:count_users(Host)
					    + Total
				  end,
				  0, ejabberd_option:hosts()),
    OutS2SNumber = ejabberd_s2s:outgoing_s2s_number(),
    InS2SNumber = ejabberd_s2s:incoming_s2s_number(),
    [?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Registered Users:")),
		     ?XAC(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(RegisteredUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Online Users:")),
		     ?XAC(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(OnlineUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Outgoing s2s Connections:")),
		     ?XAC(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(OutS2SNumber)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Incoming s2s Connections:")),
		     ?XAC(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(InS2SNumber)))])])])];
get_stats(Host, Lang) ->
    OnlineUsers =
	length(ejabberd_sm:get_vh_session_list(Host)),
    RegisteredUsers =
	ejabberd_auth:count_users(Host),
    [?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Registered Users:")),
		     ?XAC(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(RegisteredUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Online Users:")),
		     ?XAC(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(OnlineUsers)))])])])].

list_online_users(Host, _Lang) ->
    Users = [{S, U}
	     || {U, S, _R} <- ejabberd_sm:get_vh_session_list(Host)],
    SUsers = lists:usort(Users),
    lists:flatmap(fun ({_S, U} = SU) ->
			  [?AC(<<"../user/",
				 (misc:url_encode(U))/binary, "/">>,
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
            [] -> [?CT(?T("None"))];
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
                                                (misc:ip_to_list(IP))/binary,
                                                ":",
                                                (integer_to_binary(Port))/binary,
                                                "#",
                                                (misc:atom_to_binary(Node))/binary>>
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
			 ?T("Change Password"))],
    UserItems = ejabberd_hooks:run_fold(webadmin_user,
					LServer, [], [User, Server, Lang]),
    LastActivity = case ejabberd_sm:get_user_resources(User,
						       Server)
		       of
		     [] ->
			 case get_last_info(User, Server) of
			   not_found -> translate:translate(Lang, ?T("Never"));
			   {ok, Shift, _Status} ->
			       TimeStamp = {Shift div 1000000,
					    Shift rem 1000000, 0},
			       {{Year, Month, Day}, {Hour, Minute, Second}} =
				   calendar:now_to_local_time(TimeStamp),
			       (str:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
							      [Year, Month, Day,
							       Hour, Minute,
							       Second]))
			 end;
		     _ -> translate:translate(Lang, ?T("Online"))
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
	      ([?XCT(<<"h3">>, ?T("Connected Resources:"))] ++
		 FResources ++
		   [?XCT(<<"h3">>, ?T("Password:"))] ++
		     FPassword ++
		       [?XCT(<<"h3">>, ?T("Last Activity"))] ++
			 [?C(LastActivity)] ++
			   UserItems ++
			     [?P,
			      ?INPUTTD(<<"submit">>, <<"removeuser">>,
				      ?T("Remove User"))]))].

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

get_nodes(Lang) ->
    RunningNodes = ejabberd_cluster:get_nodes(),
    StoppedNodes = ejabberd_cluster:get_known_nodes()
		     -- RunningNodes,
    FRN = if RunningNodes == [] -> ?CT(?T("None"));
	     true ->
		 ?XE(<<"ul">>,
		     (lists:map(fun (N) ->
					S = iolist_to_binary(atom_to_list(N)),
					?LI([?AC(<<"../node/", S/binary, "/">>,
						 S)])
				end,
				lists:sort(RunningNodes))))
	  end,
    FSN = if StoppedNodes == [] -> ?CT(?T("None"));
	     true ->
		 ?XE(<<"ul">>,
		     (lists:map(fun (N) ->
					S = iolist_to_binary(atom_to_list(N)),
					?LI([?C(S)])
				end,
				lists:sort(StoppedNodes))))
	  end,
    [?XCT(<<"h1">>, ?T("Nodes")),
     ?XCT(<<"h3">>, ?T("Running Nodes")), FRN,
     ?XCT(<<"h3">>, ?T("Stopped Nodes")), FSN].

search_running_node(SNode) ->
    RunningNodes = ejabberd_cluster:get_nodes(),
    search_running_node(SNode, RunningNodes).

search_running_node(_, []) -> false;
search_running_node(SNode, [Node | Nodes]) ->
    case iolist_to_binary(atom_to_list(Node)) of
      SNode -> Node;
      _ -> search_running_node(SNode, Nodes)
    end.

get_node(global, Node, [], Query, Lang) ->
    Res = node_parse_query(Node, Query),
    Base = get_base_path(global, Node, 2),
    MenuItems2 = make_menu_items(global, Node, Base, Lang),
    [?XC(<<"h1">>,
	 (str:translate_and_format(Lang, ?T("Node ~p"), [Node])))]
      ++
      case Res of
	ok -> [?XREST(?T("Submitted"))];
	error -> [?XREST(?T("Bad format"))];
	nothing -> []
      end
	++
	[?XE(<<"ul">>,
	     ([?LI([?ACT(<<"db/">>, ?T("Database"))]),
	       ?LI([?ACT(<<"backup/">>, ?T("Backup"))]),
	       ?LI([?ACT(<<"stats/">>, ?T("Statistics"))]),
	       ?LI([?ACT(<<"update/">>, ?T("Update"))])]
		++ MenuItems2)),
	 ?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?INPUTT(<<"submit">>, <<"restart">>, ?T("Restart")),
	       ?C(<<" ">>),
	       ?INPUTTD(<<"submit">>, <<"stop">>, ?T("Stop"))])];
get_node(Host, Node, [], _Query, Lang) ->
    Base = get_base_path(Host, Node, 4),
    MenuItems2 = make_menu_items(Host, Node, Base, Lang),
    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("Node ~p"), [Node]))),
     ?XE(<<"ul">>, MenuItems2)];
get_node(global, Node, [<<"db">>], Query, Lang) ->
    case ejabberd_cluster:call(Node, mnesia, system_info, [tables]) of
      {badrpc, _Reason} ->
	  [?XCT(<<"h1">>, ?T("RPC Call Error"))];
      Tables ->
	  ResS = case node_db_parse_query(Node, Tables, Query) of
		   nothing -> [];
		   ok -> [?XREST(?T("Submitted"))]
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
				   MemoryB = Memory*erlang:system_info(wordsize),
				   ?XE(<<"tr">>,
				       [?XE(<<"td">>,
					  [?AC(<<"./", STable/binary,
						 "/">>,
					       STable)]),
					?XE(<<"td">>,
					    [db_storage_select(STable, Type,
							       Lang)]),
				        ?XAE(<<"td">>,
					     [{<<"class">>, <<"alignright">>}],
					      [?AC(<<"./", STable/binary,
						 "/1/">>,
					     (pretty_string_int(Size)))]),
					?XAC(<<"td">>,
					     [{<<"class">>, <<"alignright">>}],
					     (pretty_string_int(MemoryB)))])
			   end,
			   STables),
	  [?XC(<<"h1">>,
	       (str:translate_and_format(Lang, ?T("Database Tables at ~p"),
                                            [Node]))
	  )]
	    ++
	    ResS ++
	      [?XAE(<<"form">>,
		    [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		    [?XAE(<<"table">>, [],
			  [?XE(<<"thead">>,
			       [?XE(<<"tr">>,
				    [?XCT(<<"td">>, ?T("Name")),
				     ?XCT(<<"td">>, ?T("Storage Type")),
				     ?XACT(<<"td">>,
                                           [{<<"class">>, <<"alignright">>}],
                                           ?T("Elements")),
				     ?XACT(<<"td">>,
                                           [{<<"class">>, <<"alignright">>}],
                                           ?T("Memory"))])]),
			   ?XE(<<"tbody">>,
			       (Rows ++
				  [?XE(<<"tr">>,
				       [?XAE(<<"td">>,
					     [{<<"colspan">>, <<"4">>},
					      {<<"class">>, <<"alignright">>}],
					     [?INPUTT(<<"submit">>,
						      <<"submit">>,
						      ?T("Submit"))])])]))])])]
    end;
get_node(global, Node, [<<"db">>, TableName], _Query, Lang) ->
    make_table_view(Node, TableName, Lang);
get_node(global, Node, [<<"db">>, TableName, PageNumber], _Query, Lang) ->
    make_table_elements_view(Node, TableName, Lang, binary_to_integer(PageNumber));
get_node(global, Node, [<<"backup">>], Query, Lang) ->
    HomeDirRaw = case {os:getenv("HOME"), os:type()} of
		   {EnvHome, _} when is_list(EnvHome) -> list_to_binary(EnvHome);
		   {false, {win32, _Osname}} -> <<"C:/">>;
		   {false, _} -> <<"/tmp/">>
		 end,
    HomeDir = filename:nativename(HomeDirRaw),
    ResS = case node_backup_parse_query(Node, Query) of
	     nothing -> [];
	     ok -> [?XREST(?T("Submitted"))];
	     {error, Error} ->
		 [?XRES(<<(translate:translate(Lang, ?T("Error")))/binary, ": ",
			  ((str:format("~p", [Error])))/binary>>)]
	   end,
    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("Backup of ~p"), [Node])))]
      ++
      ResS ++
	[?XCT(<<"p">>,
	      ?T("Please note that these options will "
		 "only backup the builtin Mnesia database. "
		 "If you are using the ODBC module, you "
		 "also need to backup your SQL database "
		 "separately.")),
	 ?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?XAE(<<"table">>, [],
		    [?XE(<<"tbody">>,
			 [?XE(<<"tr">>,
			      [?XCT(<<"td">>, ?T("Store binary backup:")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"storepath">>,
					   (filename:join(HomeDir,
							  "ejabberd.backup")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"store">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Restore binary backup immediately:")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"restorepath">>,
					   (filename:join(HomeDir,
							  "ejabberd.backup")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"restore">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Restore binary backup after next ejabberd "
				       "restart (requires less memory):")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"fallbackpath">>,
					   (filename:join(HomeDir,
							  "ejabberd.backup")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"fallback">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>, ?T("Store plain text backup:")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"dumppath">>,
					   (filename:join(HomeDir,
							  "ejabberd.dump")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"dump">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Restore plain text backup immediately:")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"loadpath">>,
					   (filename:join(HomeDir,
							  "ejabberd.dump")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"load">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Import users data from a PIEFXIS file "
				       "(XEP-0227):")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
					   <<"import_piefxis_filepath">>,
					   (filename:join(HomeDir,
							  "users.xml")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>,
					    <<"import_piefxis_file">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Export data of all users in the server "
				       "to PIEFXIS files (XEP-0227):")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
					   <<"export_piefxis_dirpath">>,
					   HomeDir)]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>,
					    <<"export_piefxis_dir">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XE(<<"td">>,
				   [?CT(?T("Export data of users in a host to PIEFXIS "
					   "files (XEP-0227):")),
				    ?C(<<" ">>),
				    make_select_host(Lang, <<"export_piefxis_host_dirhost">>)]),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
					   <<"export_piefxis_host_dirpath">>,
					   HomeDir)]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>,
					    <<"export_piefxis_host_dir">>,
					    ?T("OK"))])]),
                          ?XE(<<"tr">>,
                              [?XE(<<"td">>,
                                   [?CT(?T("Export all tables as SQL queries "
					   "to a file:")),
                                    ?C(<<" ">>),
                                    make_select_host(Lang, <<"export_sql_filehost">>)]),
                               ?XE(<<"td">>,
				   [?INPUT(<<"text">>,
                                           <<"export_sql_filepath">>,
					   (filename:join(HomeDir,
							  "db.sql")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"export_sql_file">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Import user data from jabberd14 spool "
				       "file:")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"import_filepath">>,
					   (filename:join(HomeDir,
							  "user1.xml")))]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"import_file">>,
					    ?T("OK"))])]),
			  ?XE(<<"tr">>,
			      [?XCT(<<"td">>,
				    ?T("Import users data from jabberd14 spool "
				       "directory:")),
			       ?XE(<<"td">>,
				   [?INPUT(<<"text">>, <<"import_dirpath">>,
					   <<"/var/spool/jabber/">>)]),
			       ?XE(<<"td">>,
				   [?INPUTT(<<"submit">>, <<"import_dir">>,
					    ?T("OK"))])])])])])];
get_node(global, Node, [<<"stats">>], _Query, Lang) ->
    UpTime = ejabberd_cluster:call(Node, erlang, statistics,
		      [wall_clock]),
    UpTimeS = (str:format("~.3f",
                                           [element(1, UpTime) / 1000])),
    UpTimeDate = uptime_date(Node),
    CPUTime = ejabberd_cluster:call(Node, erlang, statistics, [runtime]),
    CPUTimeS = (str:format("~.3f",
                                            [element(1, CPUTime) / 1000])),
    OnlineUsers = ejabberd_sm:connected_users_number(),
    TransactionsCommitted = ejabberd_cluster:call(Node, mnesia,
				     system_info, [transaction_commits]),
    TransactionsAborted = ejabberd_cluster:call(Node, mnesia,
				   system_info, [transaction_failures]),
    TransactionsRestarted = ejabberd_cluster:call(Node, mnesia,
				     system_info, [transaction_restarts]),
    TransactionsLogged = ejabberd_cluster:call(Node, mnesia, system_info,
				  [transaction_log_writes]),
    [?XC(<<"h1">>,
	 (str:translate_and_format(Lang, ?T("Statistics of ~p"), [Node]))),
     ?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Uptime:")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  UpTimeS)]),
		?XE(<<"tr">>,
		    [?X(<<"td">>),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  UpTimeDate)]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("CPU Time:")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  CPUTimeS)]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Online Users:")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(OnlineUsers)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Transactions Committed:")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsCommitted)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Transactions Aborted:")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsAborted)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Transactions Restarted:")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
			  (pretty_string_int(TransactionsRestarted)))]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Transactions Logged:")),
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
	     [] -> ?CT(?T("None"));
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
					      ?T("Select All"),
					      [{<<"onClick">>,
						<<"selectAll()">>}]),
				  ?C(<<" ">>),
				  ?INPUTATTRS(<<"button">>, <<"unselectall">>,
					      ?T("Unselect All"),
					      [{<<"onClick">>,
						<<"unSelectAll()">>}])],
		 ?XAE(<<"ul">>, [{<<"class">>, <<"nolistyle">>}],
		      (BeamsLis ++ SelectButtons))
	   end,
    FmtScript = (?XC(<<"pre">>,
		     (str:format("~p", [Script])))),
    FmtLowLevelScript = (?XC(<<"pre">>,
			     (str:format("~p", [LowLevelScript])))),
    [?XC(<<"h1">>,
	 (str:translate_and_format(Lang, ?T("Update ~p"), [Node])))]
      ++
      case Res of
	ok -> [?XREST(?T("Submitted"))];
	{error, ErrorText} ->
	    [?XREST(<<"Error: ", ErrorText/binary>>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?XCT(<<"h2">>, ?T("Update plan")),
	       ?XCT(<<"h3">>, ?T("Modified modules")), Mods,
	       ?XCT(<<"h3">>, ?T("Update script")), FmtScript,
	       ?XCT(<<"h3">>, ?T("Low level update script")),
	       FmtLowLevelScript, ?XCT(<<"h3">>, ?T("Script check")),
	       ?XC(<<"pre">>, (misc:atom_to_binary(Check))),
	       ?BR,
	       ?INPUTT(<<"submit">>, <<"update">>, ?T("Update"))])];
get_node(Host, Node, NPath, Query, Lang) ->
    Res = case Host of
	      global ->
		  ejabberd_hooks:run_fold(webadmin_page_node, Host, [],
					  [Node, NPath, Query, Lang]);
	      _ ->
		  ejabberd_hooks:run_fold(webadmin_page_hostnode, Host, [],
					  [Host, Node, NPath, Query, Lang])
	  end,
    case Res of
      [] -> [?XC(<<"h1">>, <<"Not Found">>)];
      _ -> Res
    end.

uptime_date(Node) ->
    Localtime = ejabberd_cluster:call(Node, erlang, localtime, []),
    Now = calendar:datetime_to_gregorian_seconds(Localtime),
    {Wall, _} = ejabberd_cluster:call(Node, erlang, statistics, [wall_clock]),
    LastRestart = Now - (Wall div 1000),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(LastRestart),
    str:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
               [Year, Month, Day, Hour, Minute, Second]).

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

make_select_host(Lang, Name) ->
    ?XAE(<<"select">>,
	 [{<<"name">>, Name}],
	 (lists:map(fun (Host) ->
			    ?XACT(<<"option">>,
				  ([{<<"value">>, Host}]), Host)
		    end,
		    ejabberd_config:get_option(hosts)))).

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
		    [{ram_copies, ?T("RAM copy")},
		     {disc_copies, ?T("RAM and disc copy")},
		     {disc_only_copies, ?T("Disc only copy")},
		     {unknown, ?T("Remote copy")},
		     {delete_content, ?T("Delete content")},
		     {delete_table, ?T("Delete table")}]))).

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
                                                ejabberd_cluster:call(Node, ejd2sql,
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

node_update_parse_query(Node, Query) ->
    case lists:keysearch(<<"update">>, 1, Query) of
      {value, _} ->
	  ModulesToUpdateStrings =
	      proplists:get_all_values(<<"selected">>, Query),
	  ModulesToUpdate = [misc:binary_to_atom(M)
			     || M <- ModulesToUpdateStrings],
	  case ejabberd_cluster:call(Node, ejabberd_update, update,
			[ModulesToUpdate])
	      of
	    {ok, _} -> ok;
	    {error, Error} ->
		?ERROR_MSG("~p~n", [Error]),
		{error, (str:format("~p", [Error]))};
	    {badrpc, Error} ->
		?ERROR_MSG("Bad RPC: ~p~n", [Error]),
		{error,
		 <<"Bad RPC: ", ((str:format("~p", [Error])))/binary>>}
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
    <<(integer_to_binary(From))/binary, "-",
      (integer_to_binary(To))/binary, "/">>;
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

make_table_view(Node, STable, Lang) ->
    Table = misc:binary_to_atom(STable),
    TInfo = ejabberd_cluster:call(Node, mnesia, table_info, [Table, all]),
    {value, {storage_type, Type}} = lists:keysearch(storage_type, 1, TInfo),
    {value, {size, Size}} = lists:keysearch(size, 1, TInfo),
    {value, {memory, Memory}} = lists:keysearch(memory, 1, TInfo),
    MemoryB = Memory*erlang:system_info(wordsize),
    TableInfo = str:format("~p", [TInfo]),
    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("Database Tables at ~p"),
                                             [Node]))),
     ?XAE(<<"table">>, [],
	  [?XE(<<"tbody">>,
	       [?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Name")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
                          STable
                         )]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Node")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
                          misc:atom_to_binary(Node)
                         )]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Storage Type")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
                          misc:atom_to_binary(Type)
                         )]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Elements")),
                     ?XAE(<<"td">>,
                          [{<<"class">>, <<"alignright">>}],
                          [?AC(<<"1/">>,
                               (pretty_string_int(Size)))])
                    ]),
		?XE(<<"tr">>,
		    [?XCT(<<"td">>, ?T("Memory")),
		     ?XAC(<<"td">>, [{<<"class">>, <<"alignright">>}],
                          (pretty_string_int(MemoryB))
                         )])
               ])]),
     ?XC(<<"pre">>, TableInfo)].

make_table_elements_view(Node, STable, Lang, PageNumber) ->
    Table = misc:binary_to_atom(STable),
    TInfo = ejabberd_cluster:call(Node, mnesia, table_info, [Table, all]),
    {value, {storage_type, Type}} = lists:keysearch(storage_type, 1, TInfo),
    {value, {size, Size}} = lists:keysearch(size, 1, TInfo),
    PageSize = 500,
    TableContentErl = get_table_content(Node, Table, Type, PageNumber, PageSize),
    TableContent = str:format("~p", [TableContentErl]),
    PagesLinks = build_elements_pages_list(Size, PageNumber, PageSize),
    [?XC(<<"h1">>, (str:translate_and_format(Lang, ?T("Database Tables at ~p"),
                                             [Node]))),
     ?P, ?AC(<<"../">>, STable), ?P
    ] ++ PagesLinks ++ [?XC(<<"pre">>, TableContent)].

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

%%%==================================
%%%% navigation menu

make_navigation(Host, Node, Lang, JID, Level) ->
    Menu = make_navigation_menu(Host, Node, Lang, JID, Level),
    make_menu_items(Lang, Menu).

-spec make_navigation_menu(Host::global | binary(),
                           Node::cluster | atom(),
                           Lang::binary(), JID::jid(), Level::integer()) ->
    Menu::{URL::binary(), Title::binary()}
    | {URL::binary(), Title::binary(), [Menu::any()]}.
make_navigation_menu(Host, Node, Lang, JID, Level) ->
    HostNodeMenu = make_host_node_menu(Host, Node, Lang,
				       JID, Level),
    HostMenu = make_host_menu(Host, HostNodeMenu, Lang,
			      JID, Level),
    NodeMenu = make_node_menu(Host, Node, Lang, Level),
    make_server_menu(HostMenu, NodeMenu, Lang, JID, Level).

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
     HostNodeFixed2}.

make_host_menu(global, _HostNodeMenu, _Lang, _JID, _Level) ->
    {<<"">>, <<"">>, []};
make_host_menu(Host, HostNodeMenu, Lang, JID, Level) ->
    HostBase = get_base_path(Host, cluster, Level),
    HostFixed = [{<<"users">>, ?T("Users")},
		 {<<"online-users">>, ?T("Online Users")}]
		  ++
		  get_lastactivity_menuitem_list(Host) ++
		    [{<<"nodes">>, ?T("Nodes"), HostNodeMenu},
		     {<<"stats">>, ?T("Statistics")}]
		      ++ get_menu_items_hook({host, Host}, Lang),
    HostFixed2 = [Tuple
		  || Tuple <- HostFixed,
		     is_allowed_path(Host, Tuple, JID)],
    {HostBase, Host, HostFixed2}.

make_node_menu(_Host, cluster, _Lang, _Level) ->
    {<<"">>, <<"">>, []};
make_node_menu(global, Node, Lang, Level) ->
    NodeBase = get_base_path(global, Node, Level),
    NodeFixed = [{<<"db">>, ?T("Database")},
		 {<<"backup">>, ?T("Backup")},
		 {<<"stats">>, ?T("Statistics")},
		 {<<"update">>, ?T("Update")}]
		  ++ get_menu_items_hook({node, Node}, Lang),
    {NodeBase, iolist_to_binary(atom_to_list(Node)),
     NodeFixed};
make_node_menu(_Host, _Node, _Lang, _Level) ->
    {<<"">>, <<"">>, []}.

make_server_menu(HostMenu, NodeMenu, Lang, JID, Level) ->
    Base = get_base_path(global, cluster, Level),
    Fixed = [{<<"vhosts">>, ?T("Virtual Hosts"), HostMenu},
	     {<<"nodes">>, ?T("Nodes"), NodeMenu},
	     {<<"stats">>, ?T("Statistics")}]
	      ++ get_menu_items_hook(server, Lang),
    Fixed2 = [Tuple
	      || Tuple <- Fixed,
		 is_allowed_path(global, Tuple, JID)],
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

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
