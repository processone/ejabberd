%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_odbc).

-behaviour(ejabberd_sm).

%% API
-export([init/0,
	 get_session/2,
	 set_session/1,
	 delete_session/2,
	 get_sessions/0,
	 get_sessions/1,
	 get_sessions/2,
	 get_sessions/3]).

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
-include("logger.hrl").
-include("jlib.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Node = ejabberd_odbc:escape(erlang:atom_to_binary(node(), utf8)),
    lists:foldl(
      fun(Host, ok) ->
	      case ejabberd_odbc:sql_query(
		     Host, [<<"delete from sm where node='">>, Node, <<"'">>]) of
		  {updated, _} ->
		      ok;
		  Err ->
		      ?ERROR_MSG("failed to clean 'sm' table: ~p", [Err]),
		      Err
	      end;
	 (_, Err) ->
	      Err
      end, ok, ?MYHOSTS).

-spec get_session(binary(), sid()) -> {ok, #session{}} | {error, notfound}.
get_session(LServer, {Now, Pid} = SID) ->
    Host = ejabberd_odbc:escape(LServer),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    TS = now_to_timestamp(Now),
    case ejabberd_odbc:sql_query(
	   Host, [<<"select username, resource, priority, info from sm ">>,
		  <<"where usec='">>, TS, <<"' and pid='">>, PidS, <<"'">>]) of
	{selected, _, [[User, Resource, Priority, Info]|_]} ->
	    {ok, #session{sid = SID, us = {User, Resource},
			  usr = {User, Resource, LServer},
			  priority = dec_priority(Priority),
			  info = ejabberd_odbc:decode_term(Info)}};
	{selected, _, []} ->
	    {error, notfound}
    end.

set_session(#session{sid = {Now, Pid}, usr = {U, LServer, R},
		     priority = Priority, info = Info}) ->
    Username = ejabberd_odbc:escape(U),
    Resource = ejabberd_odbc:escape(R),
    InfoS = ejabberd_odbc:encode_term(Info),
    PrioS = enc_priority(Priority),
    TS = now_to_timestamp(Now),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    Node = ejabberd_odbc:escape(erlang:atom_to_binary(node(Pid), utf8)),
    case odbc_queries:update(
	   LServer,
	   <<"sm">>,
	   [<<"usec">>, <<"pid">>, <<"node">>, <<"username">>,
	    <<"resource">>, <<"priority">>, <<"info">>],
	   [TS, PidS, Node, Username, Resource, PrioS, InfoS],
	   [<<"usec='">>, TS, <<"' and pid='">>, PidS, <<"'">>]) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to update 'sm' table: ~p", [Err])
    end.

delete_session(LServer, {Now, Pid}) ->
    TS = now_to_timestamp(Now),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    case ejabberd_odbc:sql_query(
	   LServer, [<<"delete from sm where usec='">>,
		     TS, <<"' and pid='">>, PidS, <<"'">>]) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to delete from 'sm' table: ~p", [Err])
    end.

get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ?MYHOSTS).

get_sessions(LServer) ->
    case ejabberd_odbc:sql_query(
	   LServer, [<<"select usec, pid, username, ">>,
		     <<"resource, priority, info from sm">>]) of
	{selected, _, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

get_sessions(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case ejabberd_odbc:sql_query(
	   LServer, [<<"select usec, pid, username, ">>,
		     <<"resource, priority, info from sm where ">>,
		     <<"username='">>, Username, <<"'">>]) of
	{selected, _, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

get_sessions(LUser, LServer, LResource) ->
    Username = ejabberd_odbc:escape(LUser),
    Resource = ejabberd_odbc:escape(LResource),
    case ejabberd_odbc:sql_query(
	   LServer, [<<"select usec, pid, username, ">>,
		     <<"resource, priority, info from sm where ">>,
		     <<"username='">>, Username, <<"' and resource='">>,
		     Resource, <<"'">>]) of
	{selected, _, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
now_to_timestamp({MSec, Sec, USec}) ->
    erlang:integer_to_binary((MSec * 1000000 + Sec) * 1000000 + USec).

timestamp_to_now(TS) ->
    I = erlang:binary_to_integer(TS),
    Head = I div 1000000,
    USec = I rem 1000000,
    MSec = Head div 1000000,
    Sec = Head div 1000000,
    {MSec, Sec, USec}.

dec_priority(Prio) ->
    case catch erlang:binary_to_integer(Prio) of
	{'EXIT', _} ->
	    undefined;
	Int ->
	    Int
    end.

enc_priority(undefined) ->
    <<"">>;
enc_priority(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int).

row_to_session(LServer, [USec, PidS, User, Resource, PrioS, InfoS]) ->
    Now = timestamp_to_now(USec),
    Pid = erlang:list_to_pid(binary_to_list(PidS)),
    Priority = dec_priority(PrioS),
    Info = ejabberd_odbc:decode_term(InfoS),
    #session{sid = {Now, Pid}, us = {User, LServer},
	     usr = {User, LServer, Resource},
	     priority = Priority,
	     info = Info}.
