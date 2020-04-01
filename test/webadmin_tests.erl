%%%-------------------------------------------------------------------
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Created : 23 Mar 2020 by Pawel Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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
%%%-------------------------------------------------------------------
-module(webadmin_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, is_feature_advertised/3, upload_jid/1,
my_jid/1, wait_for_slave/1, wait_for_master/1,
send_recv/2, put_event/2, get_event/1]).

-include("suite.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {webadmin_single, [sequence],
     [single_test(login_page),
      single_test(welcome_page),
      single_test(user_page),
      single_test(adduser),
      single_test(changepassword),
      single_test(removeuser)]}.

login_page(Config) ->
    Headers = ?match({ok, {{"HTTP/1.1", 401, _}, Headers, _}},
		     httpc:request(get, {page(Config, ""), []}, [],
				   [{body_format, binary}]),
		     Headers),
    ?match("basic realm=\"ejabberd\"", proplists:get_value("www-authenticate", Headers, none)).

welcome_page(Config) ->
    Body = ?match({ok, {{"HTTP/1.1", 200, _}, _, Body}},
		     httpc:request(get, {page(Config, ""), [basic_auth_header(Config)]}, [],
				   [{body_format, binary}]),
		     Body),
    ?match({_, _}, binary:match(Body, <<"ejabberd Web Admin">>)).

user_page(Config) ->
    Server = ?config(server, Config),
    URL = "server/" ++ binary_to_list(Server) ++ "/user/admin/",
    Body = ?match({ok, {{"HTTP/1.1", 200, _}, _, Body}},
		  httpc:request(get, {page(Config, URL), [basic_auth_header(Config)]}, [],
				[{body_format, binary}]),
		  Body),
    ?match({_, _}, binary:match(Body, <<"<title>ejabberd Web Admin">>)).

adduser(Config) ->
    User = <<"userwebadmin-", (?config(user, Config))/binary>>,
    Server = ?config(server, Config),
    Password = ?config(password, Config),
    Body = make_query(
	     Config,
	     "server/" ++ binary_to_list(Server) ++ "/users/",
	     <<"newusername=", (mue(User))/binary, "&newuserpassword=",
	       (mue(Password))/binary, "&addnewuser=Add+User">>),
    Password = ejabberd_auth:get_password(User, Server),
    ?match({_, _}, binary:match(Body, <<"<a href='../user/">>)).

changepassword(Config) ->
    User = <<"userwebadmin-", (?config(user, Config))/binary>>,
    Server = ?config(server, Config),
    Password = <<"newpassword-", (?config(password, Config))/binary>>,
    Body = make_query(
	     Config,
	     "server/" ++ binary_to_list(Server)
	     ++ "/user/" ++ binary_to_list(mue(User)) ++ "/",
	     <<"password=", (mue(Password))/binary,
	       "&chpassword=Change+Password">>),
    ?match(Password, ejabberd_auth:get_password(User, Server)),
    ?match({_, _}, binary:match(Body, <<"<p class='result'>Submitted</p>">>)).

removeuser(Config) ->
    User = <<"userwebadmin-", (?config(user, Config))/binary>>,
    Server = ?config(server, Config),
    Body = make_query(
	     Config,
	     "server/" ++ binary_to_list(Server)
	     ++ "/user/" ++ binary_to_list(mue(User)) ++ "/",
	     <<"password=&removeuser=Remove+User">>),
    false = ejabberd_auth:user_exists(User, Server),
    ?match(nomatch, binary:match(Body, <<"<h3>Last Activity</h3>20">>)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("webadmin_" ++ atom_to_list(T)).

basic_auth_header(Config) ->
    User = <<"admin">>,
    Server = ?config(server, Config),
    Password = ?config(password, Config),
    ejabberd_auth:try_register(User, Server, Password),
    basic_auth_header(User, Server, Password).

basic_auth_header(Username, Server, Password) ->
    JidBin = <<Username/binary, "@", Server/binary, ":", Password/binary>>,
    {"authorization", "Basic " ++ base64:encode_to_string(JidBin)}.

page(Config, Tail) ->
    Server = ?config(server_host, Config),
    Port = ct:get_config(web_port, 5280),
    Url = "http://" ++ Server ++ ":" ++ integer_to_list(Port) ++ "/admin/" ++ Tail,
    case catch uri_string:normalize("/%2525") of
	"/%25" ->
	    string:replace(Url, "%25", "%2525", all);
	_ ->
	    Url
    end.

mue(Binary) ->
    misc:url_encode(Binary).

make_query(Config, URL, BodyQ) ->
    ?match({ok, {{"HTTP/1.1", 200, _}, _, Body}},
	   httpc:request(post, {page(Config, URL),
				[basic_auth_header(Config)],
				"application/x-www-form-urlencoded",
				BodyQ}, [],
			 [{body_format, binary}]),
	   Body).
