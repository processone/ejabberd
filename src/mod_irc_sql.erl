%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_irc_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_irc).

%% API
-export([init/2, get_data/3, set_data/4, import/1, import/2, export/1]).

-include("jid.hrl").
-include("mod_irc.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_data(LServer, Host, From) ->
    SJID = jid:to_string(jid:tolower(jid:remove_resource(From))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(data)s from irc_custom"
                      " where jid=%(SJID)s and host=%(Host)s")) of
        {selected, [{SData}]} ->
            mod_irc:data_to_binary(From, ejabberd_sql:decode_term(SData));
        {'EXIT', _} -> error;
        {selected, _} -> empty
    end.

set_data(LServer, Host, From, Data) ->
    SJID = jid:to_string(jid:tolower(jid:remove_resource(From))),
    SData = jlib:term_to_expr(Data),
    F = fun () ->
                ?SQL_UPSERT_T(
                   "irc_custom",
                   ["!jid=%(SJID)s",
                    "!host=%(Host)s",
                    "data=%(SData)s"]),
		ok
	end,
    ejabberd_sql:sql_transaction(LServer, F).

export(_Server) ->
    [{irc_custom,
      fun(Host, #irc_custom{us_host = {{U, S}, IRCHost},
                            data = Data}) ->
              case str:suffix(Host, IRCHost) of
                  true ->
                      SJID = jid:to_string(jid:make(U, S, <<"">>)),
                      SData = jlib:term_to_expr(Data),
                      [?SQL("delete from irc_custom"
                            " where jid=%(SJID)s and host=%(IRCHost)s;"),
                       ?SQL("insert into irc_custom(jid, host, data)"
                            " values (%(SJID)s, %(IRCHost)s, %(SData)s);")];
                  false ->
                      []
              end
      end}].

import(_LServer) ->
    [{<<"select jid, host, data from irc_custom;">>,
      fun([SJID, IRCHost, SData]) ->
              #jid{luser = U, lserver = S} = jid:from_string(SJID),
              Data = ejabberd_sql:decode_term(SData),
              #irc_custom{us_host = {{U, S}, IRCHost},
                          data = Data}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
