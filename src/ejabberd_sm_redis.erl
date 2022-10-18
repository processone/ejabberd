%%%-------------------------------------------------------------------
%%% File    : ejabberd_sm_redis.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 11 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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
	 cache_nodes/1, clean_table/1, clean_table/0]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("ejabberd_sm.hrl").
-include("logger.hrl").

-define(SM_KEY, <<"ejabberd:sm">>).
-define(MIN_REDIS_VERSION, <<"3.2.0">>).
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
    NodeHostKey = node_host_to_key(node(), element(2, Session#session.us)),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(USKey, SIDKey, T),
		   ejabberd_redis:hset(ServKey, USSIDKey, T),
		   ejabberd_redis:hset(NodeHostKey,
				       <<USKey/binary, "||", SIDKey/binary>>,
				       USSIDKey),
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
    NodeHostKey = node_host_to_key(node(), element(2, Session#session.us)),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hdel(USKey, [SIDKey]),
		   ejabberd_redis:hdel(ServKey, [USSIDKey]),
		   ejabberd_redis:hdel(NodeHostKey,
				       [<<USKey/binary, "||", SIDKey/binary>>]),
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
    case clean_table() of
	ok -> {ok, #state{}};
	{error, Why} -> {stop, Why}
    end.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({redis_message, ?SM_KEY, Data}, State) ->
    case binary_to_term(Data) of
	{delete, Key} ->
	    ets_cache:delete(?SM_CACHE, Key);
	Msg ->
	    ?WARNING_MSG("Unexpected redis message: ~p", [Msg])
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
us_to_key({LUser, LServer}) ->
    <<(?SM_KEY)/binary, ":", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
    <<(?SM_KEY)/binary, ":", LServer/binary>>.

us_sid_to_key(US, SID) ->
    term_to_binary({US, SID}).

sid_to_key(SID) ->
    term_to_binary(SID).

node_session_deletion_cursor(Node, Host) ->
    NodeName = node_host_to_key(Node, Host),
    <<NodeName/binary, ":deletioncursor">>.

node_host_to_key(Node, Host) when is_atom(Node) ->
    NodeBin = atom_to_binary(node(), utf8),
    node_host_to_key(NodeBin, Host);
node_host_to_key(NodeBin, Host) ->
    HostKey = server_to_key(Host),
    <<HostKey/binary, ":node:", NodeBin/binary>>.

decode_session_list(Vals) ->
  [binary_to_term(Val) || {_, Val} <- Vals].

clean_table() ->
    clean_table(node()).

clean_table(Node) when is_atom(Node) ->
    clean_table(atom_to_binary(Node, utf8));
clean_table(Node) ->
    ?DEBUG("Cleaning Redis SM table... ", []),
    try
	lists:foreach(
	  fun(Host) ->
		  ok = clean_node_sessions(Node, Host)
	  end, ejabberd_sm:get_vh_by_backend(?MODULE))
    catch _:{badmatch, {error, _} = Err} ->
	    ?ERROR_MSG("Failed to clean Redis SM table", []),
	    Err
    end.

clean_node_sessions(Node, Host) ->
    case load_script() of
        {ok, SHA} ->
            clean_node_sessions(Node, Host, SHA);
        Err ->
            Err
    end.

clean_node_sessions(Node, Host, SHA) ->
    Keys = [node_host_to_key(Node, Host),
	    server_to_key(Host),
	    node_session_deletion_cursor(Node, Host)],
    case ejabberd_redis:evalsha(SHA, Keys, [1000]) of
	{ok, <<"0">>} ->
	    ok;
	{ok, _Cursor} ->
	    clean_node_sessions(Node, Host, SHA);
	{error, _} = Err ->
	    Err
    end.

load_script() ->
    case misc:read_lua("redis_sm.lua") of
	{ok, Data} ->
	    case ejabberd_redis:info(server) of
		{ok, Info} ->
		    case proplists:get_value(redis_version, Info) of
			V when V >= ?MIN_REDIS_VERSION ->
			    ejabberd_redis:script_load(Data);
			V ->
			    ?CRITICAL_MSG("Unsupported Redis version: ~ts. "
					  "The version must be ~ts or above",
					  [V, ?MIN_REDIS_VERSION]),
			    {error, unsupported_redis_version}
		    end;
		{error, _} = Err ->
		    Err
	    end;
	{error, _} = Err ->
	    Err
    end.
