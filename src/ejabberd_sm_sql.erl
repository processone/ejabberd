%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015-2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

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
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Node = jlib:atom_to_binary(node()),
    ?INFO_MSG("Cleaning SQL SM table...", []),
    lists:foldl(
      fun(Host, ok) ->
	      case ejabberd_sql:sql_query(
		     Host, ?SQL("delete from sm where node=%(Node)s")) of
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
    InfoS = jlib:term_to_expr(Info),
    PrioS = enc_priority(Priority),
    TS = now_to_timestamp(Now),
    PidS = list_to_binary(erlang:pid_to_list(Pid)),
    Node = jlib:atom_to_binary(node(Pid)),
    case ?SQL_UPSERT(LServer, "sm",
                     ["!usec=%(TS)d",
                      "!pid=%(PidS)s",
                      "node=%(Node)s",
                      "username=%(U)s",
                      "resource=%(R)s",
                      "priority=%(PrioS)s",
                      "info=%(InfoS)s"]) of
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
	   ?SQL("select @(usec)d, @(pid)s, @(username)s,"
                " @(resource)s, @(priority)s, @(info)s "
                "from sm where usec=%(TS)d and pid=%(PidS)s")) of
	{selected, [Row]} ->
            ejabberd_sql:sql_query(
              LServer,
              ?SQL("delete from sm"
                   " where usec=%(TS)d and pid=%(PidS)s")),
	    {ok, row_to_session(LServer, Row)};
	{selected, []} ->
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
	   LServer,
           ?SQL("select @(usec)d, @(pid)s, @(username)s,"
                " @(resource)s, @(priority)s, @(info)s from sm")) of
	{selected, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

get_sessions(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
           ?SQL("select @(usec)d, @(pid)s, @(username)s,"
                " @(resource)s, @(priority)s, @(info)s from sm"
                " where username=%(LUser)s")) of
	{selected, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

get_sessions(LUser, LServer, LResource) ->
    case ejabberd_sql:sql_query(
	   LServer,
           ?SQL("select @(usec)d, @(pid)s, @(username)s,"
                " @(resource)s, @(priority)s, @(info)s from sm"
                " where username=%(LUser)s and resource=%(LResource)s")) of
	{selected, Rows} ->
	    [row_to_session(LServer, Row) || Row <- Rows];
	Err ->
	    ?ERROR_MSG("failed to select from 'sm' table: ~p", [Err]),
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
now_to_timestamp({MSec, Sec, USec}) ->
    (MSec * 1000000 + Sec) * 1000000 + USec.

timestamp_to_now(I) ->
    Head = I div 1000000,
    USec = I rem 1000000,
    MSec = Head div 1000000,
    Sec = Head div 1000000,
    {MSec, Sec, USec}.

dec_priority(Prio) ->
    case catch binary_to_integer(Prio) of
	{'EXIT', _} ->
	    undefined;
	Int ->
	    Int
    end.

enc_priority(undefined) ->
    <<"">>;
enc_priority(Int) when is_integer(Int) ->
    integer_to_binary(Int).

row_to_session(LServer, {USec, PidS, User, Resource, PrioS, InfoS}) ->
    Now = timestamp_to_now(USec),
    Pid = erlang:list_to_pid(binary_to_list(PidS)),
    Priority = dec_priority(PrioS),
    Info = ejabberd_sql:decode_term(InfoS),
    #session{sid = {Now, Pid}, us = {User, LServer},
	     usr = {User, LServer, Resource},
	     priority = Priority,
	     info = Info}.
