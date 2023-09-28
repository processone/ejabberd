%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  4 Dec 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-module(mod_mix_pam_sql).
-behaviour(mod_mix_pam).

%% API
-export([init/2, add_channel/3, get_channel/2,
	 get_channels/1, del_channel/2, del_channels/1]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, schemas()),
    ok.

schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"mix_pam">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"channel">>, type = text},
                     #sql_column{name = <<"service">>, type = text},
                     #sql_column{name = <<"id">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"username">>, <<"server_host">>,
                                         <<"channel">>, <<"service">>],
                              unique = true}]}]}].

add_channel(User, Channel, ID) ->
    {LUser, LServer, _} = jid:tolower(User),
    {Chan, Service, _} = jid:tolower(Channel),
    case ?SQL_UPSERT(LServer, "mix_pam",
		     ["!channel=%(Chan)s",
		      "!service=%(Service)s",
		      "!username=%(LUser)s",
		      "!server_host=%(LServer)s",
		      "id=%(ID)s"]) of
	ok -> ok;
	_Err -> {error, db_failure}
    end.

get_channel(User, Channel) ->
    {LUser, LServer, _} = jid:tolower(User),
    {Chan, Service, _} = jid:tolower(Channel),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(id)s from mix_pam where "
		"channel=%(Chan)s and service=%(Service)s "
		"and username=%(LUser)s and %(LServer)H")) of
	{selected, [{ID}]} -> {ok, ID};
	{selected, []} -> {error, notfound};
	_Err -> {error, db_failure}
    end.

get_channels(User) ->
    {LUser, LServer, _} = jid:tolower(User),
    SQL = ?SQL("select @(channel)s, @(service)s, @(id)s from mix_pam "
	       "where username=%(LUser)s and %(LServer)H"),
    case ejabberd_sql:sql_query(LServer, SQL) of
	{selected, Ret} ->
	    {ok, lists:filtermap(
		   fun({Chan, Service, ID}) ->
			   case jid:make(Chan, Service) of
			       error ->
				   report_corrupted(SQL),
				   false;
			       JID ->
				   {true, {JID, ID}}
			   end
		   end, Ret)};
	_Err ->
	    {error, db_failure}
    end.

del_channel(User, Channel) ->
    {LUser, LServer, _} = jid:tolower(User),
    {Chan, Service, _} = jid:tolower(Channel),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from mix_pam where "
		"channel=%(Chan)s and service=%(Service)s "
		"and username=%(LUser)s and %(LServer)H")) of
	{updated, _} -> ok;
	_Err -> {error, db_failure}
    end.

del_channels(User) ->
    {LUser, LServer, _} = jid:tolower(User),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from mix_pam where "
		"username=%(LUser)s and %(LServer)H")) of
	{updated, _} -> ok;
	_Err -> {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec report_corrupted(#sql_query{}) -> ok.
report_corrupted(SQL) ->
    ?ERROR_MSG("Corrupted values returned by SQL request: ~ts",
	       [SQL#sql_query.hash]).
