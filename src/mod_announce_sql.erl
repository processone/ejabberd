%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_announce_sql).
-behaviour(mod_announce).

%% API
-export([init/2, set_motd_users/2, set_motd/2, delete_motd/1,
	 get_motd/1, is_motd_user/2, set_motd_user/2, import/1,
	 import/2, export/1]).

-include("jlib.hrl").
-include("mod_announce.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

set_motd_users(LServer, USRs) ->
    F = fun() ->
		lists:foreach(
		  fun({U, _S, _R}) ->
			  Username = ejabberd_sql:escape(U),
			  sql_queries:update_t(
			    <<"motd">>,
			    [<<"username">>, <<"xml">>],
			    [Username, <<"">>],
			    [<<"username='">>, Username, <<"'">>])
		  end, USRs)
	end,
    ejabberd_sql:sql_transaction(LServer, F).

set_motd(LServer, Packet) ->
    XML = ejabberd_sql:escape(fxml:element_to_binary(Packet)),
    F = fun() ->
		sql_queries:update_t(
		  <<"motd">>,
		  [<<"username">>, <<"xml">>],
		  [<<"">>, XML],
		  [<<"username=''">>])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

delete_motd(LServer) ->
    F = fun() ->
		ejabberd_sql:sql_query_t([<<"delete from motd;">>])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

get_motd(LServer) ->
    case catch ejabberd_sql:sql_query(
                 LServer, [<<"select xml from motd where username='';">>]) of
        {selected, [<<"xml">>], [[XML]]} ->
            case fxml_stream:parse_element(XML) of
                {error, _} ->
                    error;
                Packet ->
		    {ok, Packet}
	    end;
	_ ->
	    error
    end.

is_motd_user(LUser, LServer) ->
    Username = ejabberd_sql:escape(LUser),
    case catch ejabberd_sql:sql_query(
		 LServer,
		 [<<"select username from motd "
		    "where username='">>, Username, <<"';">>]) of
	{selected, [<<"username">>], [_|_]} ->
	    true;
	_ ->
	    false
    end.

set_motd_user(LUser, LServer) ->
    Username = ejabberd_sql:escape(LUser),
    F = fun() ->
		sql_queries:update_t(
		  <<"motd">>,
		  [<<"username">>, <<"xml">>],
		  [Username, <<"">>],
		  [<<"username='">>, Username, <<"'">>])
	end,
    ejabberd_sql:sql_transaction(LServer, F).

export(_Server) ->
    [{motd,
      fun(Host, #motd{server = LServer, packet = El})
            when LServer == Host ->
              [[<<"delete from motd where username='';">>],
               [<<"insert into motd(username, xml) values ('', '">>,
                ejabberd_sql:escape(fxml:element_to_binary(El)),
                <<"');">>]];
         (_Host, _R) ->
              []
      end},
     {motd_users,
      fun(Host, #motd_users{us = {LUser, LServer}})
            when LServer == Host, LUser /= <<"">> ->
              Username = ejabberd_sql:escape(LUser),
              [[<<"delete from motd where username='">>, Username, <<"';">>],
               [<<"insert into motd(username, xml) values ('">>,
                Username, <<"', '');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select xml from motd where username='';">>,
      fun([XML]) ->
              El = fxml_stream:parse_element(XML),
              #motd{server = LServer, packet = El}
      end},
     {<<"select username from motd where xml='';">>,
      fun([LUser]) ->
              #motd_users{us = {LUser, LServer}}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
