%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_mnesia).

-behaviour(gen_server).
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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
-include("jlib.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
	{ok, _Pid} ->
	    ok;
	Err ->
	    Err
    end.

-spec get_session(binary(), sid()) -> {ok, #session{}} | {error, notfound}.
get_session(_LServer, SID) ->
    case mnesia:dirty_read(session, SID) of
	[] ->
	    {error, notfound};
	[Session] ->
	    {ok, Session}
    end.

-spec set_session(#session{}) -> ok.
set_session(Session) ->
    mnesia:dirty_write(Session).

-spec delete_session(binary(), sid()) -> ok.
delete_session(_LServer, SID) ->
    mnesia:dirty_delete(session, SID).

-spec get_sessions() -> [#session{}].
get_sessions() ->
    ets:tab2list(session).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$_']}]).

-spec get_sessions(binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer) ->
    mnesia:dirty_index_read(session, {LUser, LServer}, #session.us).

-spec get_sessions(binary(), binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer, LResource) ->
    mnesia:dirty_index_read(session, {LUser, LServer, LResource}, #session.usr).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    update_tables(),
    mnesia:create_table(session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session)}]),
    mnesia:create_table(session_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session_counter)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:add_table_copy(session_counter, node(), ram_copies),
    mnesia:subscribe(system),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    ets:select_delete(
      session,
      ets:fun2ms(
	fun(#session{sid = {_, Pid}}) ->
		node(Pid) == Node
	end)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [ur, user, node] -> mnesia:delete_table(session);
      [ur, user, pid] -> mnesia:delete_table(session);
      [usr, us, pid] -> mnesia:delete_table(session);
      [usr, us, sid, priority, info] -> mnesia:delete_table(session);
      [sid, usr, us, priority] ->
	  mnesia:delete_table(session);
      [sid, usr, us, priority, info] -> ok;
      {'EXIT', _} -> ok
    end,
    case lists:member(presence, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(presence);
      false -> ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.
