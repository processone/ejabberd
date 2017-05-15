%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_riak.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Authentification via Riak
%%% Created : 12 Nov 2012 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(ejabberd_auth_riak).

-compile([{parse_transform, ejabberd_sql_pt}]).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, stop/1, set_password/3, try_register/3,
	 get_users/2, count_users/2,
	 get_password/2, remove_user/2, store_type/1, export/1, import/2,
	 plain_password_required/1]).
-export([passwd_schema/0]).

-include("ejabberd.hrl").
-include("ejabberd_sql_pt.hrl").

-record(passwd, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                 password = <<"">> :: binary() | scram() | '_'}).

start(_Host) ->
    ok.

stop(_Host) ->
    ok.

plain_password_required(Server) ->
    store_type(Server) == scram.

store_type(Server) ->
    ejabberd_auth:password_format(Server).

passwd_schema() ->
    {record_info(fields, passwd), #passwd{}}.

set_password(User, Server, Password) ->
    ejabberd_riak:put(#passwd{us = {User, Server}, password = Password},
		      passwd_schema(),
		      [{'2i', [{<<"host">>, Server}]}]).

try_register(User, Server, Password) ->
    US = {User, Server},
    case ejabberd_riak:get(passwd, passwd_schema(), US) of
	{error, notfound} ->
	    ejabberd_riak:put(#passwd{us = US, password = Password},
			      passwd_schema(),
			      [{'2i', [{<<"host">>, Server}]}]);
	{ok, _} ->
	    {error, exists};
	{error, _} = Err ->
	    Err
    end.

get_users(Server, _) ->
    case ejabberd_riak:get_keys_by_index(passwd, <<"host">>, Server) of
        {ok, Users} ->
            Users;
        _ ->
            []
    end.

count_users(Server, _) ->
    case ejabberd_riak:count_by_index(passwd, <<"host">>, Server) of
        {ok, N} ->
            N;
        _ ->
            0
    end.

get_password(User, Server) ->
    case ejabberd_riak:get(passwd, passwd_schema(), {User, Server}) of
	{ok, Password} ->
	    {ok, Password};
	{error, _} ->
	    error
    end.

remove_user(User, Server) ->
    ejabberd_riak:delete(passwd, {User, Server}).

export(_Server) ->
    [{passwd,
      fun(Host, #passwd{us = {LUser, LServer}, password = Password})
         when LServer == Host ->
              [?SQL("delete from users where username=%(LUser)s;"),
               ?SQL("insert into users(username, password) "
                    "values (%(LUser)s, %(Password)s);")];
         (_Host, _R) ->
              []
      end}].

import(LServer, [LUser, Password, _TimeStamp]) ->
    Passwd = #passwd{us = {LUser, LServer}, password = Password},
    ejabberd_riak:put(Passwd, passwd_schema(), [{'2i', [{<<"host">>, LServer}]}]).
