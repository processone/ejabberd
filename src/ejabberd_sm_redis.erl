%%%-------------------------------------------------------------------
%%% File    : ejabberd_sm_redis.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 11 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(ejabberd_sm_redis).
-ifndef(GEN_SERVER).
-define(GEN_SERVER, p1_server).
-endif.
-behaviour(?GEN_SERVER).

-behaviour(ejabberd_sm).

-export([init/0, set_session/1, delete_session/1,
	 get_sessions/0, get_sessions/1, get_sessions/2,
	 cache_nodes/1]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
-include("logger.hrl").

-define(SM_KEY, <<"ejabberd:sm">>).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    Spec = {?MODULE, {?MODULE, start_link, []},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	Err -> Err
    end.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec cache_nodes(binary()) -> [node()].
cache_nodes(_LServer) ->
    [node()].

-spec set_session(#session{}) -> ok | {error, ejabberd_redis:error_reason()}.
set_session(Session) ->
    T = term_to_binary(Session),
    USKey = us_to_key(Session#session.us),
    SIDKey = sid_to_key(Session#session.sid),
    ServKey = server_to_key(element(2, Session#session.us)),
    USSIDKey = us_sid_to_key(Session#session.us, Session#session.sid),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(USKey, SIDKey, T),
		   ejabberd_redis:hset(ServKey, USSIDKey, T),
		   ejabberd_redis:publish(
		     ?SM_KEY, term_to_binary({delete, Session#session.us}))
	   end) of
	{ok, _} ->
	    ok;
	Err ->
	    Err
    end.

-spec delete_session(#session{}) -> ok | {error, ejabberd_redis:error_reason()}.
delete_session(#session{sid = SID} = Session) ->
    USKey = us_to_key(Session#session.us),
    SIDKey = sid_to_key(SID),
    ServKey = server_to_key(element(2, Session#session.us)),
    USSIDKey = us_sid_to_key(Session#session.us, SID),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hdel(USKey, [SIDKey]),
		   ejabberd_redis:hdel(ServKey, [USSIDKey]),
		   ejabberd_redis:publish(
		     ?SM_KEY,
		     term_to_binary({delete, Session#session.us}))
	   end) of
	{ok, _} ->
	    ok;
	Err ->
	    Err
    end.

-spec get_sessions() -> [#session{}].
get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    ServKey = server_to_key(LServer),
    case ejabberd_redis:hgetall(ServKey) of
	{ok, Vals} ->
	    decode_session_list(Vals);
	{error, _} ->
	    []
    end.

-spec get_sessions(binary(), binary()) -> {ok, [#session{}]} |
					  {error, ejabberd_redis:error_reason()}.
get_sessions(LUser, LServer) ->
    USKey = us_to_key({LUser, LServer}),
    case ejabberd_redis:hgetall(USKey) of
	{ok, Vals} ->
	    {ok, decode_session_list(Vals)};
	Err ->
	    Err
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ejabberd_redis:subscribe([?SM_KEY]),
    clean_table(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({redis_message, ?SM_KEY, Data}, State) ->
    case binary_to_term(Data) of
	{delete, Key} ->
	    ets_cache:delete(?SM_CACHE, Key);
	Msg ->
	    ?WARNING_MSG("unexpected redis message: ~p", [Msg])
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
us_to_key({LUser, LServer}) ->
    <<"ejabberd:sm:", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
    <<"ejabberd:sm:", LServer/binary>>.

us_sid_to_key(US, SID) ->
    term_to_binary({US, SID}).

sid_to_key(SID) ->
    term_to_binary(SID).

decode_session_list(Vals) ->
  [binary_to_term(Val) || {_, Val} <- Vals].

clean_table() ->
    ?DEBUG("Cleaning Redis SM table...", []),
    try
	lists:foreach(
	  fun(LServer) ->
		  ServKey = server_to_key(LServer),
		  {ok, Vals} = ejabberd_redis:hkeys(ServKey),
		  {ok, _} =
		      ejabberd_redis:multi(
			fun() ->
				lists:foreach(
				  fun(USSIDKey) ->
					  {US, SID} = binary_to_term(USSIDKey),
					  if node(element(2, SID)) == node() ->
						  USKey = us_to_key(US),
						  SIDKey = sid_to_key(SID),
						  ejabberd_redis:hdel(ServKey, [USSIDKey]),
						  ejabberd_redis:hdel(USKey, [SIDKey]);
					     true ->
						  ok
					  end
				  end, Vals)
			end)
	  end, ejabberd_sm:get_vh_by_backend(?MODULE))
    catch _:{badmatch, {error, _}} ->
	    ?ERROR_MSG("failed to clean redis c2s sessions", [])
    end.
