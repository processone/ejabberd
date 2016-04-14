%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_irc_sql).

-behaviour(mod_irc).

%% API
-export([init/2, get_data/3, set_data/4, import/1, import/2, export/1]).

-include("jlib.hrl").
-include("mod_irc.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_data(LServer, Host, From) ->
    LJID = jid:tolower(jid:remove_resource(From)),
    SJID = ejabberd_odbc:escape(jid:to_string(LJID)),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(
		 LServer,
		 [<<"select data from irc_custom where jid='">>,
		  SJID, <<"' and host='">>, SHost,
		  <<"';">>]) of
	{selected, [<<"data">>], [[SData]]} ->
	    mod_irc:data_to_binary(From, ejabberd_odbc:decode_term(SData));
	{'EXIT', _} -> error;
	{selected, _, _} -> empty
    end.

set_data(LServer, Host, From, Data) ->
    LJID = jid:tolower(jid:remove_resource(From)),
    SJID = ejabberd_odbc:escape(jid:to_string(LJID)),
    SHost = ejabberd_odbc:escape(Host),
    SData = ejabberd_odbc:encode_term(Data),
    F = fun () ->
		odbc_queries:update_t(<<"irc_custom">>,
				      [<<"jid">>, <<"host">>, <<"data">>],
				      [SJID, SHost, SData],
				      [<<"jid='">>, SJID, <<"' and host='">>,
				       SHost, <<"'">>]),
		ok
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

export(_Server) ->
    [{irc_custom,
      fun(Host, #irc_custom{us_host = {{U, S}, IRCHost},
                            data = Data}) ->
              case str:suffix(Host, IRCHost) of
                  true ->
                      SJID = ejabberd_odbc:escape(
                               jid:to_string(
                                 jid:make(U, S, <<"">>))),
                      SIRCHost = ejabberd_odbc:escape(IRCHost),
                      SData = ejabberd_odbc:encode_term(Data),
                      [[<<"delete from irc_custom where jid='">>, SJID,
                        <<"' and host='">>, SIRCHost, <<"';">>],
                       [<<"insert into irc_custom(jid, host, "
                          "data) values ('">>,
                        SJID, <<"', '">>, SIRCHost, <<"', '">>, SData,
                        <<"');">>]];
                  false ->
                      []
              end
      end}].

import(_LServer) ->
    [{<<"select jid, host, data from irc_custom;">>,
      fun([SJID, IRCHost, SData]) ->
              #jid{luser = U, lserver = S} = jid:from_string(SJID),
              Data = ejabberd_odbc:decode_term(SData),
              #irc_custom{us_host = {{U, S}, IRCHost},
                          data = Data}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
