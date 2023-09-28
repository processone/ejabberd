%%%-------------------------------------------------------------------
%%% Created :  1 Dec 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_mix_sql).
-behaviour(mod_mix).

%% API
-export([init/2]).
-export([set_channel/6, get_channels/2, get_channel/3, del_channel/3]).
-export([set_participant/6, get_participant/4, get_participants/3, del_participant/4]).
-export([subscribe/5, unsubscribe/4, unsubscribe/5, get_subscribed/4]).

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
                name = <<"mix_channel">>,
                columns =
                    [#sql_column{name = <<"channel">>, type = text},
                     #sql_column{name = <<"service">>, type = text},
                     #sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"domain">>, type = text},
                     #sql_column{name = <<"jid">>, type = text},
                     #sql_column{name = <<"hidden">>, type = boolean},
                     #sql_column{name = <<"hmac_key">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"channel">>, <<"service">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"service">>]}]},
             #sql_table{
                name = <<"mix_participant">>,
                columns =
                    [#sql_column{name = <<"channel">>, type = text},
                     #sql_column{name = <<"service">>, type = text},
                     #sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"domain">>, type = text},
                     #sql_column{name = <<"jid">>, type = text},
                     #sql_column{name = <<"id">>, type = text},
                     #sql_column{name = <<"nick">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"channel">>, <<"service">>,
                                         <<"username">>, <<"domain">>],
                              unique = true}]},
             #sql_table{
                name = <<"mix_subscription">>,
                columns =
                    [#sql_column{name = <<"channel">>, type = text},
                     #sql_column{name = <<"service">>, type = {text, 75}},
                     #sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"domain">>, type = {text, 75}},
                     #sql_column{name = <<"node">>, type = text},
                     #sql_column{name = <<"jid">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"channel">>, <<"service">>,
                                         <<"username">>, <<"domain">>,
                                         <<"node">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"channel">>, <<"service">>,
                                         <<"node">>]}]}]}].

set_channel(LServer, Channel, Service, CreatorJID, Hidden, Key) ->
    {User, Domain, _} = jid:tolower(CreatorJID),
    RawJID = jid:encode(jid:remove_resource(CreatorJID)),
    case ?SQL_UPSERT(LServer, "mix_channel",
		     ["!channel=%(Channel)s",
		      "!service=%(Service)s",
		      "username=%(User)s",
		      "domain=%(Domain)s",
		      "jid=%(RawJID)s",
		      "hidden=%(Hidden)b",
		      "hmac_key=%(Key)s"]) of
	ok -> ok;
	_Err -> {error, db_failure}
    end.

get_channels(LServer, Service) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(channel)s, @(hidden)b from mix_channel "
		"where service=%(Service)s")) of
	{selected, Ret} ->
	    {ok, [Channel || {Channel, Hidden} <- Ret, Hidden == false]};
	_Err ->
	    {error, db_failure}
    end.

get_channel(LServer, Channel, Service) ->
    SQL = ?SQL("select @(jid)s, @(hidden)b, @(hmac_key)s from mix_channel "
	       "where channel=%(Channel)s and service=%(Service)s"),
    case ejabberd_sql:sql_query(LServer, SQL) of
	{selected, [{RawJID, Hidden, Key}]} ->
	    try jid:decode(RawJID) of
		JID -> {ok, {JID, Hidden, Key}}
	    catch _:{bad_jid, _} ->
		    report_corrupted(jid, SQL),
		    {error, db_failure}
	    end;
	{selected, []} -> {error, notfound};
	_Err -> {error, db_failure}
    end.

del_channel(LServer, Channel, Service) ->
    F = fun() ->
		ejabberd_sql:sql_query_t(
		  ?SQL("delete from mix_channel where "
		       "channel=%(Channel)s and service=%(Service)s")),
		ejabberd_sql:sql_query_t(
		  ?SQL("delete from mix_participant where "
		       "channel=%(Channel)s and service=%(Service)s")),
		ejabberd_sql:sql_query_t(
		  ?SQL("delete from mix_subscription where "
		       "channel=%(Channel)s and service=%(Service)s"))
	end,
    case ejabberd_sql:sql_transaction(LServer, F) of
	{atomic, _} -> ok;
	_Err -> {error, db_failure}
    end.

set_participant(LServer, Channel, Service, JID, ID, Nick) ->
    {User, Domain, _} = jid:tolower(JID),
    RawJID = jid:encode(jid:remove_resource(JID)),
    case ?SQL_UPSERT(LServer, "mix_participant",
		     ["!channel=%(Channel)s",
		      "!service=%(Service)s",
		      "!username=%(User)s",
		      "!domain=%(Domain)s",
		      "jid=%(RawJID)s",
		      "id=%(ID)s",
		      "nick=%(Nick)s"]) of
	ok -> ok;
	_Err -> {error, db_failure}
    end.

get_participant(LServer, Channel, Service, JID) ->
    {User, Domain, _} = jid:tolower(JID),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(id)s, @(nick)s from mix_participant "
		"where channel=%(Channel)s and service=%(Service)s "
		"and username=%(User)s and domain=%(Domain)s")) of
	{selected, [Ret]} -> {ok, Ret};
	{selected, []} -> {error, notfound};
	_Err -> {error, db_failure}
    end.

get_participants(LServer, Channel, Service) ->
    SQL = ?SQL("select @(jid)s, @(id)s, @(nick)s from mix_participant "
	       "where channel=%(Channel)s and service=%(Service)s"),
    case ejabberd_sql:sql_query(LServer, SQL) of
	{selected, Ret} ->
	    {ok, lists:filtermap(
		   fun({RawJID, ID, Nick}) ->
			   try jid:decode(RawJID) of
			       JID -> {true, {JID, ID, Nick}}
			   catch _:{bad_jid, _} ->
				   report_corrupted(jid, SQL),
				   false
			   end
		   end, Ret)};
	_Err ->
	    {error, db_failure}
    end.

del_participant(LServer, Channel, Service, JID) ->
    {User, Domain, _} = jid:tolower(JID),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from mix_participant where "
		"channel=%(Channel)s and service=%(Service)s "
		"and username=%(User)s and domain=%(Domain)s")) of
	{updated, _} -> ok;
	_Err -> {error, db_failure}
    end.

subscribe(_LServer, _Channel, _Service, _JID, []) ->
    ok;
subscribe(LServer, Channel, Service, JID, Nodes) ->
    {User, Domain, _} = jid:tolower(JID),
    RawJID = jid:encode(jid:remove_resource(JID)),
    F = fun() ->
		lists:foreach(
		  fun(Node) ->
			  ?SQL_UPSERT_T(
			     "mix_subscription",
			     ["!channel=%(Channel)s",
			      "!service=%(Service)s",
			      "!username=%(User)s",
			      "!domain=%(Domain)s",
			      "!node=%(Node)s",
			      "jid=%(RawJID)s"])
		  end, Nodes)
	end,
    case ejabberd_sql:sql_transaction(LServer, F) of
	{atomic, _} -> ok;
	_Err -> {error, db_failure}
    end.

get_subscribed(LServer, Channel, Service, Node) ->
    SQL = ?SQL("select @(jid)s from mix_subscription "
	       "where channel=%(Channel)s and service=%(Service)s "
	       "and node=%(Node)s"),
    case ejabberd_sql:sql_query(LServer, SQL) of
	{selected, Ret} ->
	    {ok, lists:filtermap(
		   fun({RawJID}) ->
			   try jid:decode(RawJID) of
			       JID -> {true, JID}
			   catch _:{bad_jid, _} ->
				   report_corrupted(jid, SQL),
				   false
			   end
		   end, Ret)};
	_Err ->
	    {error, db_failure}
    end.

unsubscribe(LServer, Channel, Service, JID) ->
    {User, Domain, _} = jid:tolower(JID),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("delete from mix_subscription "
		"where channel=%(Channel)s and service=%(Service)s "
		"and username=%(User)s and domain=%(Domain)s")) of
	{updated, _} -> ok;
	_Err -> {error, db_failure}
    end.

unsubscribe(_LServer, _Channel, _Service, _JID, []) ->
    ok;
unsubscribe(LServer, Channel, Service, JID, Nodes) ->
    {User, Domain, _} = jid:tolower(JID),
    F = fun() ->
		lists:foreach(
		  fun(Node) ->
			  ejabberd_sql:sql_query_t(
			    ?SQL("delete from mix_subscription "
				 "where channel=%(Channel)s "
				 "and service=%(Service)s "
				 "and username=%(User)s "
				 "and domain=%(Domain)s "
				 "and node=%(Node)s"))
		  end, Nodes)
	end,
    case ejabberd_sql:sql_transaction(LServer, F) of
	{atomic, ok} -> ok;
	_Err -> {error, db_failure}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec report_corrupted(atom(), #sql_query{}) -> ok.
report_corrupted(Column, SQL) ->
    ?ERROR_MSG("Corrupted value of '~ts' column returned by "
	       "SQL request: ~ts", [Column, SQL#sql_query.hash]).
