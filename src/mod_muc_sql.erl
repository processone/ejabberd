%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_muc_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_muc).

%% API
-export([init/2, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4,
	 import/1, import/2, export/1]).

-include("jid.hrl").
-include("mod_muc.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

store_room(LServer, Host, Name, Opts) ->
    SOpts = jlib:term_to_expr(Opts),
    F = fun () ->
		?SQL_UPSERT_T(
                   "muc_room",
                   ["!name=%(Name)s",
                    "!host=%(Host)s",
                    "opts=%(SOpts)s"])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

restore_room(LServer, Host, Name) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(opts)s from muc_room where name=%(Name)s"
                      " and host=%(Host)s")) of
	{selected, [{Opts}]} ->
	    mod_muc:opts_to_binary(ejabberd_sql:decode_term(Opts));
	_ ->
	    error
    end.

forget_room(LServer, Host, Name) ->
    F = fun () ->
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from muc_room where name=%(Name)s"
                       " and host=%(Host)s"))
	end,
    ejabberd_sql:sql_transaction(LServer, F).

can_use_nick(LServer, Host, JID, Nick) ->
    SJID = jid:to_string(jid:tolower(jid:remove_resource(JID))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(jid)s from muc_registered "
                      "where nick=%(Nick)s"
                      " and host=%(Host)s")) of
	{selected, [{SJID1}]} -> SJID == SJID1;
	_ -> true
    end.

get_rooms(LServer, Host) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(name)s, @(opts)s from muc_room"
                      " where host=%(Host)s")) of
	{selected, RoomOpts} ->
	    lists:map(
	      fun({Room, Opts}) ->
		      #muc_room{name_host = {Room, Host},
				opts = mod_muc:opts_to_binary(
					 ejabberd_sql:decode_term(Opts))}
	      end, RoomOpts);
	Err ->
	    ?ERROR_MSG("failed to get rooms: ~p", [Err]),
	    []
    end.

get_nick(LServer, Host, From) ->
    SJID = jid:to_string(jid:tolower(jid:remove_resource(From))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(nick)s from muc_registered where"
                      " jid=%(SJID)s and host=%(Host)s")) of
	{selected, [{Nick}]} -> Nick;
	_ -> error
    end.

set_nick(LServer, Host, From, Nick) ->
    JID = jid:to_string(jid:tolower(jid:remove_resource(From))),
    F = fun () ->
		case Nick of
		    <<"">> ->
			ejabberd_sql:sql_query_t(
			  ?SQL("delete from muc_registered where"
                               " jid=%(JID)s and host=%(Host)s")),
			ok;
		    _ ->
			Allow = case ejabberd_sql:sql_query_t(
				       ?SQL("select @(jid)s from muc_registered"
                                            " where nick=%(Nick)s"
                                            " and host=%(Host)s")) of
				    {selected, [{J}]} -> J == JID;
				    _ -> true
				end,
			if Allow ->
				?SQL_UPSERT_T(
                                  "muc_registered",
                                  ["!jid=%(JID)s",
                                   "!host=%(Host)s",
                                   "nick=%(Nick)s"]),
				ok;
			   true ->
				false
			end
		end
	end,
    ejabberd_sql:sql_transaction(LServer, F).

export(_Server) ->
    [{muc_room,
      fun(Host, #muc_room{name_host = {Name, RoomHost}, opts = Opts}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SOpts = jlib:term_to_expr(Opts),
                      [?SQL("delete from muc_room where name=%(Name)s"
                            " and host=%(RoomHost)s;"),
                       ?SQL("insert into muc_room(name, host, opts) "
                            "values ("
                            "%(Name)s, %(RoomHost)s, %(SOpts)s);")];
                  false ->
                      []
              end
      end},
     {muc_registered,
      fun(Host, #muc_registered{us_host = {{U, S}, RoomHost},
                                nick = Nick}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SJID = jid:to_string(jid:make(U, S, <<"">>)),
                      [?SQL("delete from muc_registered where"
                            " jid=%(SJID)s and host=%(RoomHost)s;"),
                       ?SQL("insert into muc_registered(jid, host, "
                            "nick) values ("
                            "%(SJID)s, %(RoomHost)s, %(Nick)s);")];
                  false ->
                      []
              end
      end}].

import(_LServer) ->
    [{<<"select name, host, opts from muc_room;">>,
      fun([Name, RoomHost, SOpts]) ->
              Opts = mod_muc:opts_to_binary(ejabberd_sql:decode_term(SOpts)),
              #muc_room{name_host = {Name, RoomHost}, opts = Opts}
      end},
     {<<"select jid, host, nick from muc_registered;">>,
      fun([J, RoomHost, Nick]) ->
              #jid{user = U, server = S} = jid:from_string(J),
              #muc_registered{us_host = {{U, S}, RoomHost},
                              nick = Nick}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
