%%%-------------------------------------------------------------------
%%% File    : mod_muc_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_muc_sql).


-behaviour(mod_muc).
-behaviour(mod_muc_room).
-behaviour(ejabberd_db_serialize).

%% API
-export([init/2, store_room/5, store_changes/4,
         restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, get_nicks/2, set_nick/4,
	 import/3, export/1]).
-export([register_online_room/4, unregister_online_room/4, find_online_room/3,
	 get_online_rooms/3, count_online_rooms/2, rsm_supported/0,
	 register_online_user/4, unregister_online_user/4,
	 count_online_rooms_by_user/3, get_online_rooms_by_user/3,
	 get_subscribed_rooms/3, get_rooms_without_subscribers/2,
	 get_hibernated_rooms_older_than/3,
	 find_online_room_by_pid/2, remove_user/2]).
-export([set_affiliation/6, set_affiliations/4, get_affiliation/5,
	 get_affiliations/3, search_affiliation/4]).
-export([sql_schemas/0]).
-export([serialize/3, deserialize_start/1, deserialize/2]).

-include_lib("xmpp/include/jid.hrl").
-include("mod_muc.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("ejabberd_db_serialize.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()),
    case gen_mod:ram_db_mod(Opts, mod_muc) of
	?MODULE ->
	    clean_tables(Host);
	_ ->
	    ok
    end.

sql_schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"muc_room">>,
                columns =
                    [#sql_column{name = <<"name">>, type = text},
                     #sql_column{name = <<"host">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"opts">>, type = {text, big}},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"name">>, <<"host">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"host">>, <<"created_at">>]}]},
             #sql_table{
                name = <<"muc_registered">>,
                columns =
                    [#sql_column{name = <<"jid">>, type = text},
                     #sql_column{name = <<"host">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"nick">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"jid">>, <<"host">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"nick">>]}]},
             #sql_table{
                name = <<"muc_online_room">>,
                columns =
                    [#sql_column{name = <<"name">>, type = text},
                     #sql_column{name = <<"host">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"node">>, type = text},
                     #sql_column{name = <<"pid">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"name">>, <<"host">>],
                              unique = true}]},
             #sql_table{
                name = <<"muc_online_users">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server">>, type = {text, 75}},
                     #sql_column{name = <<"resource">>, type = text},
                     #sql_column{name = <<"name">>, type = text},
                     #sql_column{name = <<"host">>, type = {text, 75}},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"node">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"username">>, <<"server">>,
                                         <<"resource">>, <<"name">>,
                                         <<"host">>],
                              unique = true}]},
             #sql_table{
                name = <<"muc_room_subscribers">>,
                columns =
                    [#sql_column{name = <<"room">>, type = text},
                     #sql_column{name = <<"host">>, type = text},
                     #sql_column{name = <<"jid">>, type = text},
                     #sql_column{name = <<"nick">>, type = text},
                     #sql_column{name = <<"nodes">>, type = text},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"host">>, <<"room">>, <<"jid">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"host">>, <<"jid">>]},
                           #sql_index{
                              columns = [<<"jid">>]}]}]}].

store_room(LServer, Host, Name, Opts, ChangesHints) ->
    {Subs, Opts2} = case lists:keytake(subscribers, 1, Opts) of
			{value, {subscribers, S}, OptN} -> {S, OptN};
			_ -> {[], Opts}
		    end,
    SOpts = misc:term_to_expr(Opts2),
    Timestamp = case lists:keyfind(hibernation_time, 1, Opts) of
                    false -> {{1970, 1, 2}, {0, 0, 0}};
                    {_, undefined} -> {{1970, 1, 2}, {0, 0, 0}};
		    {_, Time} -> usec_to_sql_timestamp(Time)
		end,
    F = fun () ->
		?SQL_UPSERT_T(
                   "muc_room",
                   ["!name=%(Name)s",
                    "!host=%(Host)s",
                    "server_host=%(LServer)s",
                    "opts=%(SOpts)s",
		    "created_at=%(Timestamp)t"]),
                case ChangesHints of
                    Changes when is_list(Changes) ->
                        [change_room(Host, Name, Change) || Change <- Changes];
                    _ ->
                        ejabberd_sql:sql_query_t(
                          ?SQL("delete from muc_room_subscribers where "
                               "room=%(Name)s and host=%(Host)s")),
                        [change_room(Host, Name, {add_subscription, JID, Nick, Nodes})
                         || {JID, Nick, Nodes} <- Subs]
                end
	end,
    ejabberd_sql:sql_transaction(LServer, F).

store_changes(LServer, Host, Name, Changes) ->
    F = fun () ->
                [change_room(Host, Name, Change) || Change <- Changes]
	end,
    ejabberd_sql:sql_transaction(LServer, F).

change_room(Host, Room, {add_subscription, JID, Nick, Nodes}) ->
    SJID = jid:encode(JID),
    SNodes = misc:term_to_expr(Nodes),
    ?SQL_UPSERT_T(
       "muc_room_subscribers",
       ["!jid=%(SJID)s",
	"!host=%(Host)s",
	"!room=%(Room)s",
	"nick=%(Nick)s",
	"nodes=%(SNodes)s"]);
change_room(Host, Room, {del_subscription, JID}) ->
    SJID = jid:encode(JID),
    ejabberd_sql:sql_query_t(?SQL("delete from muc_room_subscribers where "
				  "room=%(Room)s and host=%(Host)s and jid=%(SJID)s"));
change_room(Host, Room, Change) ->
    ?ERROR_MSG("Unsupported change on room ~ts@~ts: ~p", [Room, Host, Change]).

restore_room(LServer, Host, Name) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(opts)s from muc_room where name=%(Name)s"
                      " and host=%(Host)s")) of
	{selected, [{Opts}]} ->
	    OptsD = ejabberd_sql:decode_term(Opts),
	    case catch ejabberd_sql:sql_query(
		LServer,
		?SQL("select @(jid)s, @(nick)s, @(nodes)s from muc_room_subscribers where room=%(Name)s"
		     " and host=%(Host)s")) of
		{selected, []} ->
		    OptsR = mod_muc:opts_to_binary(OptsD),
		    case lists:keymember(subscribers, 1, OptsD) of
			true ->
			    store_room(LServer, Host, Name, OptsR, undefined);
			_ ->
			    ok
		    end,
		    OptsR;
		{selected, Subs} ->
		    SubData = lists:map(
			     fun({Jid, Nick, Nodes}) ->
				 {jid:decode(Jid), Nick, ejabberd_sql:decode_term(Nodes)}
			     end, Subs),
		    Opts2 = lists:keystore(subscribers, 1, OptsD, {subscribers, SubData}),
		    mod_muc:opts_to_binary(Opts2);
		_ ->
		    {error, db_failure}
	    end;
	{selected, _} ->
            error;
	_ ->
	    {error, db_failure}
    end.

forget_room(LServer, Host, Name) ->
    F = fun () ->
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from muc_room where name=%(Name)s"
                       " and host=%(Host)s")),
		ejabberd_sql:sql_query_t(
		    ?SQL("delete from muc_room_subscribers where room=%(Name)s"
                       " and host=%(Host)s"))
	end,
    ejabberd_sql:sql_transaction(LServer, F).

can_use_nick(LServer, ServiceOrRoom, JID, Nick) ->
    SJID = jid:encode(jid:tolower(jid:remove_resource(JID))),
    SqlQuery = case (jid:decode(ServiceOrRoom))#jid.lserver of
                   ServiceOrRoom ->
                       ?SQL("select @(jid)s from muc_registered "
                            "where nick=%(Nick)s"
                            " and host=%(ServiceOrRoom)s");
                   Service ->
                       ?SQL("select @(jid)s from muc_registered "
                            "where nick=%(Nick)s"
                            " and (host=%(ServiceOrRoom)s or host=%(Service)s)")
               end,
    case catch ejabberd_sql:sql_query(LServer, SqlQuery) of
	{selected, [{SJID1}]} -> SJID == SJID1;
	_ -> true
    end.

get_rooms_without_subscribers(LServer, Host) ->
    case catch ejabberd_sql:sql_query(
	LServer,
	?SQL("select @(name)s, @(opts)s from muc_room"
	     " where host=%(Host)s")) of
	{selected, RoomOpts} ->
	    lists:map(
		fun({Room, Opts}) ->
		    OptsD = ejabberd_sql:decode_term(Opts),
		    #muc_room{name_host = {Room, Host},
			      opts = mod_muc:opts_to_binary(OptsD)}
		end, RoomOpts);
	_Err ->
	    []
    end.

get_hibernated_rooms_older_than(LServer, Host, Timestamp) ->
    TimestampS = usec_to_sql_timestamp(Timestamp),
    case catch ejabberd_sql:sql_query(
	LServer,
	?SQL("select @(name)s, @(opts)s from muc_room"
	     " where host=%(Host)s and created_at < %(TimestampS)t and created_at > '1970-01-02 00:00:00'")) of
	{selected, RoomOpts} ->
	    lists:map(
		fun({Room, Opts}) ->
		    OptsD = ejabberd_sql:decode_term(Opts),
		    #muc_room{name_host = {Room, Host},
			      opts = mod_muc:opts_to_binary(OptsD)}
		end, RoomOpts);
	_Err ->
	    []
    end.

get_rooms(LServer, Host) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(name)s, @(opts)s from muc_room"
                      " where host=%(Host)s")) of
	{selected, RoomOpts} ->
	    case catch ejabberd_sql:sql_query(
		LServer,
		?SQL("select @(room)s, @(jid)s, @(nick)s, @(nodes)s from muc_room_subscribers"
		     " where host=%(Host)s")) of
		{selected, Subs} ->
		    SubsD = lists:foldl(
			fun({Room, Jid, Nick, Nodes}, Dict) ->
                                Sub = {jid:decode(Jid),
                                       Nick, ejabberd_sql:decode_term(Nodes)},
                                maps:update_with(
                                  Room,
                                  fun(SubAcc) ->
                                          [Sub | SubAcc]
                                  end,
                                  [Sub],
                                  Dict)
			end, maps:new(), Subs),
	    lists:map(
	      fun({Room, Opts}) ->
			    OptsD = ejabberd_sql:decode_term(Opts),
			    OptsD2 = case {maps:find(Room, SubsD), lists:keymember(subscribers, 1, OptsD)} of
					 {_, true} ->
					     store_room(LServer, Host, Room, mod_muc:opts_to_binary(OptsD), undefined),
					     OptsD;
					 {{ok, SubsI}, false} ->
					     lists:keystore(subscribers, 1, OptsD, {subscribers, SubsI});
					 _ ->
					     OptsD
			    end,
		      #muc_room{name_host = {Room, Host},
				      opts = mod_muc:opts_to_binary(OptsD2)}
	      end, RoomOpts);
	_Err ->
		    []
	    end;
	_Err ->
	    []
    end.

get_nick(LServer, Host, From) ->
    SJID = jid:encode(jid:tolower(jid:remove_resource(From))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(nick)s from muc_registered where"
                      " jid=%(SJID)s and host=%(Host)s")) of
	{selected, [{Nick}]} -> Nick;
	_ -> error
    end.

get_nicks(LServer, Host) ->
    case catch ejabberd_sql:sql_query(LServer,
                                      ?SQL("select @(jid)s, @(nick)s from muc_registered where"
                                           " host=%(Host)s"))
    of
        {selected, Results} ->
            lists:map(fun({JidBin, Nick}) ->
                         {User, Server, _Resource} =
                             jid:tolower(
                                 jid:decode(JidBin)),
                         {User, Server, Nick}
                      end,
                      Results);
        _ ->
            error
    end.

set_nick(LServer, ServiceOrRoom, From, Nick) ->
    JID = jid:encode(jid:tolower(jid:remove_resource(From))),
    F = fun () ->
		case Nick of
		    <<"">> ->
			ejabberd_sql:sql_query_t(
			  ?SQL("delete from muc_registered where"
                               " jid=%(JID)s and host=%(ServiceOrRoom)s")),
			ok;
		    _ ->
			Service = (jid:decode(ServiceOrRoom))#jid.lserver,
                        SqlQuery = case (ServiceOrRoom == Service) of
                                       true ->
                                           ?SQL("select @(jid)s, @(host)s from muc_registered "
                                                "where nick=%(Nick)s"
                                                " and host=%(ServiceOrRoom)s");
                                       false ->
                                           ?SQL("select @(jid)s, @(host)s from muc_registered "
                                                "where nick=%(Nick)s"
                                                " and (host=%(ServiceOrRoom)s or host=%(Service)s)")
                                   end,
			Allow = case ejabberd_sql:sql_query_t(SqlQuery) of
				    {selected, []}
                                      when (ServiceOrRoom == Service) ->
                                        %% Registering in the service...
                                        %% check if nick is registered for some room in this service
                                        {selected, NickRegistrations} =
                                            ejabberd_sql:sql_query_t(
                                              ?SQL("select @(jid)s, @(host)s from muc_registered "
                                                   "where nick=%(Nick)s")),
                                        not lists:any(fun({_NRJid, NRServiceOrRoom}) ->
                                                              Service == (jid:decode(NRServiceOrRoom))#jid.lserver end,
                                                      NickRegistrations);
				    {selected, []} ->
                                        %% Nick not registered in any service or room
                                        true;
				    {selected, [{_J, Host}]}
                                      when (Host == Service) and (ServiceOrRoom /= Service) ->
                                        %% Registering in a room, but the nick is already registered in the service
                                        false;
				    {selected, [{J, _Host}]} ->
                                        %% Registering in room (or service) a nick that is
                                        %% already registered in this room (or service)
                                        %% Only the owner of this registration can use the nick
                                        J == JID
				end,
			if Allow ->
				?SQL_UPSERT_T(
                                  "muc_registered",
                                  ["!jid=%(JID)s",
                                   "!host=%(ServiceOrRoom)s",
                                   "server_host=%(LServer)s",
                                   "nick=%(Nick)s"]),
				ok;
			   true ->
				false
			end
		end
	end,
    ejabberd_sql:sql_transaction(LServer, F).

set_affiliation(_ServerHost, _Room, _Host, _JID, _Affiliation, _Reason) ->
    {error, not_implemented}.

set_affiliations(_ServerHost, _Room, _Host, _Affiliations) ->
    {error, not_implemented}.

get_affiliation(_ServerHost, _Room, _Host, _LUser, _LServer) ->
    {error, not_implemented}.

get_affiliations(_ServerHost, _Room, _Host) ->
    {error, not_implemented}.

search_affiliation(_ServerHost, _Room, _Host, _Affiliation) ->
    {error, not_implemented}.

register_online_room(ServerHost, Room, Host, Pid) ->
    PidS = misc:encode_pid(Pid),
    NodeS = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(ServerHost,
		     "muc_online_room",
		     ["!name=%(Room)s",
		      "!host=%(Host)s",
                      "server_host=%(ServerHost)s",
		      "node=%(NodeS)s",
		      "pid=%(PidS)s"]) of
	ok ->
	    ok;
	Err ->
	    Err
    end.

unregister_online_room(ServerHost, Room, Host, Pid) ->
    %% TODO: report errors
    PidS = misc:encode_pid(Pid),
    NodeS = erlang:atom_to_binary(node(Pid), latin1),
    ejabberd_sql:sql_query(
      ServerHost,
      ?SQL("delete from muc_online_room where name=%(Room)s and "
	   "host=%(Host)s and node=%(NodeS)s and pid=%(PidS)s")).

find_online_room(ServerHost, Room, Host) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(pid)s, @(node)s from muc_online_room where "
		"name=%(Room)s and host=%(Host)s")) of
	{selected, [{PidS, NodeS}]} ->
	    try {ok, misc:decode_pid(PidS, NodeS)}
	    catch _:{bad_node, _} -> error
	    end;
	{selected, []} ->
	    error;
	_Err ->
	    error
    end.

find_online_room_by_pid(ServerHost, Pid) ->
    PidS = misc:encode_pid(Pid),
    NodeS = erlang:atom_to_binary(node(Pid), latin1),
    case ejabberd_sql:sql_query(
	ServerHost,
	?SQL("select @(name)s, @(host)s from muc_online_room where "
	     "node=%(NodeS)s and pid=%(PidS)s")) of
	{selected, [{Room, Host}]} ->
	    {ok, Room, Host};
	{selected, []} ->
	    error;
	_Err ->
	    error
    end.

count_online_rooms(ServerHost, Host) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(count(*))d from muc_online_room "
		"where host=%(Host)s")) of
	{selected, [{Num}]} ->
	    Num;
	_Err ->
	    0
    end.

get_online_rooms(ServerHost, Host, _RSM) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(name)s, @(pid)s, @(node)s from muc_online_room "
		"where host=%(Host)s")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun({Room, PidS, NodeS}) ->
		      try [{Room, Host, misc:decode_pid(PidS, NodeS)}]
		      catch _:{bad_node, _} -> []
		      end
	      end, Rows);
	_Err ->
	    []
    end.

rsm_supported() ->
    false.

register_online_user(ServerHost, {U, S, R}, Room, Host) ->
    NodeS = erlang:atom_to_binary(node(), latin1),
    case ?SQL_UPSERT(ServerHost, "muc_online_users",
		     ["!username=%(U)s",
		      "!server=%(S)s",
		      "!resource=%(R)s",
		      "!name=%(Room)s",
		      "!host=%(Host)s",
                      "server_host=%(ServerHost)s",
		      "node=%(NodeS)s"]) of
	ok ->
	    ok;
	Err ->
	    Err
    end.

unregister_online_user(ServerHost, {U, S, R}, Room, Host) ->
    %% TODO: report errors
    ejabberd_sql:sql_query(
      ServerHost,
      ?SQL("delete from muc_online_users where username=%(U)s and "
	   "server=%(S)s and resource=%(R)s and name=%(Room)s and "
	   "host=%(Host)s")).

count_online_rooms_by_user(ServerHost, U, S) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(count(*))d from muc_online_users where "
		"username=%(U)s and server=%(S)s")) of
	{selected, [{Num}]} ->
	    Num;
	_Err ->
	    0
    end.

get_online_rooms_by_user(ServerHost, U, S) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(name)s, @(host)s from muc_online_users where "
		"username=%(U)s and server=%(S)s")) of
	{selected, Rows} ->
	    Rows;
	_Err ->
	    []
    end.

export(_Server) ->
    [{muc_room,
      fun(Host, #muc_room{name_host = {Name, RoomHost}, opts = Opts}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SOpts = misc:term_to_expr(Opts),
                      [?SQL("delete from muc_room where name=%(Name)s"
                            " and host=%(RoomHost)s;"),
                       ?SQL_INSERT(
                          "muc_room",
                          ["name=%(Name)s",
                           "host=%(RoomHost)s",
                           "server_host=%(Host)s",
                           "opts=%(SOpts)s"])];
                  false ->
                      []
              end
      end},
     {muc_registered,
      fun(Host, #muc_registered{us_host = {{U, S}, RoomHost},
                                nick = Nick}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SJID = jid:encode(jid:make(U, S)),
                      [?SQL("delete from muc_registered where"
                            " jid=%(SJID)s and host=%(RoomHost)s;"),
                       ?SQL_INSERT(
                          "muc_registered",
                          ["jid=%(SJID)s",
                           "host=%(RoomHost)s",
                           "server_host=%(Host)s",
                           "nick=%(Nick)s"])];
                  false ->
                      []
              end
      end}].

import(_, _, _) ->
    ok.

serialize(LServer, BatchSize, undefined) ->
    serialize(LServer, BatchSize, {rooms, 0});
serialize(LServer, BatchSize, {rooms, Offset}) ->
    Records =
    case ejabberd_sql:sql_query(
	LServer,
	?SQL("select @(name)s, @(host)s, @(opts)s from muc_room "
	     "where %(LServer)H "
	     "order by host, name "
	     "limit %(BatchSize)d offset %(Offset)d")) of
	{selected, Data} ->
	    lists:foldl(
		fun(_, {error, _} = Err) -> Err;
		   ({Name, Host, Opts}, Acc) ->
		       case ejabberd_sql:sql_query(
			   LServer,
			   ?SQL("select @(jid)s, @(nick)s, @(nodes)s from muc_room_subscribers where room=%(Name)s"
				" and host=%(Host)s and %(LServer)H")) of
			   {selected, Subs} ->
			       SubData = lists:map(
				   fun({Jid, Nick, Nodes}) ->
				       {jid:decode(Jid), Nick, ejabberd_sql:decode_term(Nodes)}
				   end, Subs),
			       OptsD = ejabberd_sql:decode_term(Opts),
			       Opts2 = lists:keystore(subscribers, 1, OptsD, {subscribers, SubData}),
			       [#serialize_muc_room_v1{serverhost = LServer, host = Host, name = Name,
						       options = mod_muc:opts_to_binary(Opts2)} | Acc];
			   _ ->
			       {error, io_lib:format("Error when retrieving list of muc subscribers for room ~s@~s",
						     [Name, Host])}
		       end
		end, [], Data);
	_ ->
	    {error, io_lib:format("Error when retrieving list of muc rooms",
				  [])}
    end,
    case Records of
	{error, _} = Err -> Err;
	_ ->
	    case length(Records) of
		Val when Val < BatchSize ->
		    case serialize(LServer, BatchSize - Val, {registrations, 0}) of
			{ok, Records2, Next} ->
			    {ok, Records ++ Records2, Next};
			Err -> Err
		    end;
		Val ->
		    {ok, Records, {rooms, Offset + Val}}
	    end
    end;
serialize(LServer, BatchSize, {registrations, Offset}) ->
    Records =
	case ejabberd_sql:sql_query(
	    LServer,
	    ?SQL("select @(jid)s, @(host)s, @(nick)s from muc_registered "
		 "where %(LServer)H "
		 "order by host, jid "
		 "limit %(BatchSize)d offset %(Offset)d")) of
	    {selected, Data} ->
		lists:map(
		    fun({Jid, Host, Nick}) ->
			#serialize_muc_registrations_v1{serverhost = LServer, host = Host, jid = Jid, nick = Nick}
		    end, Data);
	    _ ->
		{error, io_lib:format("Error when retrieving list of muc registered nicks",
				      [])}
	end,
    {ok, Records, {registrations, Offset + length(Records)}}.

deserialize_start(LServer) ->
    ejabberd_sql:sql_query(
	LServer,
	?SQL("delete from muc_room where %(LServer)H")),
    ejabberd_sql:sql_query(
	LServer,
	?SQL("delete from muc_registered where %(LServer)H")).

deserialize(LServer, Batch) ->
    lists:foldl(
	fun(_, {error, _} = Err) ->
	    Err;
	   (#serialize_muc_room_v1{name = Name, host = Host, options = Opts}, _) ->
	       case store_room(LServer, Host, Name, Opts, undefined) of
		   {atomic, _} ->
		       ok;
		   _ -> {error, io_lib:format("Error when writing muc room data", [])}
	       end;
	   (#serialize_muc_registrations_v1{host = Host, jid = Jid, nick = Nick}, _) ->
	       case ejabberd_sql:sql_query(LServer,
					   ?SQL_INSERT(
					       "muc_registered",
					       ["jid=%(Jid)s",
						"host=%(Host)s",
						"server_host=%(LServer)s",
						"nick=%(Nick)s"])) of
		   {updated, _} -> ok;
		   _ -> {error, io_lib:format("Error when writing muc registration data", [])}
	       end
	end, ok, Batch).

get_subscribed_rooms(LServer, Host, Jid) ->
    JidS = jid:encode(Jid),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(room)s, @(nick)s, @(nodes)s from muc_room_subscribers "
		"where jid=%(JidS)s and host=%(Host)s")) of
	{selected, Subs} ->
	    {ok, [{jid:make(Room, Host), Nick, ejabberd_sql:decode_term(Nodes)}
		  || {Room, Nick, Nodes} <- Subs]};
	_Error ->
	    {error, db_failure}
    end.

remove_user(LUser, LServer) ->
    SJID = jid:encode(jid:make(LUser, LServer)),
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from muc_room_subscribers where jid=%(SJID)s")),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_tables(ServerHost) ->
    NodeS = erlang:atom_to_binary(node(), latin1),
    ?DEBUG("Cleaning SQL muc_online_room table...", []),
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("delete from muc_online_room where node=%(NodeS)s")) of
	{updated, _} ->
	    ok;
	Err1 ->
	    ?ERROR_MSG("Failed to clean 'muc_online_room' table: ~p", [Err1]),
	    Err1
    end,
    ?DEBUG("Cleaning SQL muc_online_users table...", []),
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("delete from muc_online_users where node=%(NodeS)s")) of
	{updated, _} ->
	    ok;
	Err2 ->
	    ?ERROR_MSG("Failed to clean 'muc_online_users' table: ~p", [Err2]),
	    Err2
    end.

usec_to_sql_timestamp(Timestamp) ->
    TS = misc:usec_to_now(Timestamp),
    calendar:now_to_universal_time(TS).
