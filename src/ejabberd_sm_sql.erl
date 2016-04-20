%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015-2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_sql).

-behaviour(ejabberd_sm).

%% API
-export([init/0,
	 set_session/1,
	 delete_session/4,
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
    Node = ejabberd_sql:escape(jlib:atom_to_binary(node())),
    ?INFO_MSG("Cleaning SQL SM table...", []),
    lists:foldl(
      fun(Host, ok) ->
	      case ejabberd_sql:sql_query(
		     Host, [<<"delete from sm where node='">>, Node, <<"'">>]) of
		  {updated, _} ->
		      ok;
		  Err ->
		      ?ERROR_MSG("failed to clean 'sm' table: ~p", [Err]),
		      Err
	      end;
	 (_, Err) ->
	      Err
      end, ok, ejabberd_sm:get_vh_by_backend(?MODULE)).

set_session(#session{sid = {Now, Pid}, usr = {U, LServer, R},
		     priority = Priority, info = Info}) ->
    Username = ejabberd_sql:escape(U),
    Resource = ejabberd_sql:escape(R),
    InfoS = ejabberd_sql:encode_term(Info),
    PrioS = enc_priority(Priority),
    TS = now_to_timestamp(Now),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    Node = ejabberd_sql:escape(jlib:atom_to_binary(node(Pid))),
    case sql_queries:update(
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

delete_session(_LUser, LServer, _LResource, {Now, Pid}) ->
    TS = now_to_timestamp(Now),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    case ejabberd_sql:sql_query(
	   LServer,
	   [<<"select usec, pid, username, resource, priority, info ">>,
	    <<"from sm where usec='">>, TS, <<"' and pid='">>,PidS, <<"'">>]) of
	{selected, _, [Row]} ->
	    ejabberd_sql:sql_query(
	      LServer, [<<"delete from sm where usec='">>,
			TS, <<"' and pid='">>, PidS, <<"'">>]),
	    {ok, row_to_session(LServer, Row)};
	{selected, _, []} ->
	    {error, notfound};
	Err ->
	    ?ERROR_MSG("failed to delete from 'sm' table: ~p", [Err]),
	    {error, notfound}
    end.

get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

get_sessions(LServer) ->
    case ejabberd_sql:sql_query(
	   LServer, [<<"select usec, pid, username, ">>,
		     <<"resource, priority, info from sm">>]) of
	{selected, _, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

get_sessions(LUser, LServer) ->
    Username = ejabberd_sql:escape(LUser),
    case ejabberd_sql:sql_query(
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
    Username = ejabberd_sql:escape(LUser),
    Resource = ejabberd_sql:escape(LResource),
    case ejabberd_sql:sql_query(
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
    jlib:integer_to_binary((MSec * 1000000 + Sec) * 1000000 + USec).

timestamp_to_now(TS) ->
    I = jlib:binary_to_integer(TS),
    Head = I div 1000000,
    USec = I rem 1000000,
    MSec = Head div 1000000,
    Sec = Head div 1000000,
    {MSec, Sec, USec}.

dec_priority(Prio) ->
    case catch jlib:binary_to_integer(Prio) of
	{'EXIT', _} ->
	    undefined;
	Int ->
	    Int
    end.

enc_priority(undefined) ->
    <<"">>;
enc_priority(Int) when is_integer(Int) ->
    jlib:integer_to_binary(Int).

row_to_session(LServer, [USec, PidS, User, Resource, PrioS, InfoS]) ->
    Now = timestamp_to_now(USec),
    Pid = erlang:list_to_pid(binary_to_list(PidS)),
    Priority = dec_priority(PrioS),
    Info = ejabberd_sql:decode_term(InfoS),
    #session{sid = {Now, Pid}, us = {User, LServer},
	     usr = {User, LServer, Resource},
	     priority = Priority,
	     info = Info}.
