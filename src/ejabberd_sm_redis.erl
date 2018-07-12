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
-define(DELETION_CURSOR_TIMEOUT_SEC, "30").
-behaviour(ejabberd_sm).

-export([init/0, set_session/1, delete_session/1,
	 get_sessions/0, get_sessions/1, get_sessions/2,
	 cache_nodes/1, clean_table/1, clean_table/0]).
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
    NodeHostKey = node_host_to_key(node(), element(2, Session#session.us)),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(USKey, SIDKey, T),
		   ejabberd_redis:hset(ServKey, USSIDKey, T),
           ejabberd_redis:hset(NodeHostKey , <<USKey/binary, "||", SIDKey/binary>>, USSIDKey),
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
           ejabberd_redis:hdel(NodeHostKey, [<<USKey/binary, "||", SIDKey/binary>>]),
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
    ?ERROR_MSG("unexpected info: ~p in cluster server", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
us_to_key({LUser, LServer}) ->
    SMPrefixKey = ?SM_KEY, 
    <<SMPrefixKey/binary, ":", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
    SMPrefixKey = ?SM_KEY, 
    <<SMPrefixKey/binary, ":", LServer/binary>>.

us_sid_to_key(US, SID) ->
    term_to_binary({US, SID}).

sid_to_key(SID) ->
    term_to_binary(SID).

node_session_deletion_cursor(Node, Host) when is_binary(Host) and is_binary(Node)  ->
    NodeName = node_host_to_key(Node, Host),
    <<NodeName/binary, ":deletioncursor">>.

node_host_to_key(Node, Host) when is_atom(Node) and is_binary(Host) ->
    NodeBin = atom_to_binary(node(), utf8),
    node_host_to_key(NodeBin, Host);
node_host_to_key(NodeBin, Host) when is_binary(NodeBin) and is_binary(Host) ->
    HostKey = server_to_key(Host),
    <<HostKey/binary, ":node:", NodeBin/binary>>;
node_host_to_key(_NodeBin, _Host) ->
    ?ERROR_MSG("Invalid node type ", []).

decode_session_list(Vals) ->
  [binary_to_term(Val) || {_, Val} <- Vals].

clean_table() ->
    clean_table(node()).

clean_table(Node) when is_atom(Node) ->
    clean_table(atom_to_binary(Node, utf8));

clean_table(Node) when is_binary(Node) ->
    ?DEBUG("Cleaning Redis SM table... ", []),
    try
        lists:foreach(
            fun(Host) -> clean_node_sessions(Node, Host) end, 
            ejabberd_sm:get_vh_by_backend(?MODULE)
        ),
        ok
    catch E:R ->
	    ?ERROR_MSG("failed to clean redis c2s sessions due to ~p: ~p", [E, R]),
        {error, R}
    end;

clean_table(_) ->
    ?ERROR_MSG("Wrong node data type in clean table call ", []).

clean_node_sessions(Node, Host) ->
    case load_script() of 
        {ok , SHA} -> 
            clean_node_sessions(Node, Host, SHA);
        Error ->
            ?ERROR_MSG("Failure in generating the SHA ~p", [Error])
    end.

clean_node_sessions(Node, Host, SHA) ->
    ?INFO_MSG("Cleaning node sessions for node ~p with host ~p ", [Node, Host]),
    case ejabberd_redis:q(["EVALSHA", SHA,
        3,
        node_host_to_key(Node, Host),
        server_to_key(Host), 
        node_session_deletion_cursor(Node, Host),
        1000
        ]) of 
            {ok, <<"0">>} ->
                ?DEBUG("Cleaned node sessions for node ~p with host ~p ", [Node, Host]);
            {ok, Cursor} ->
                ?DEBUG("Cleaning redis sessions with cursor ~p ", [Cursor]),
                clean_node_sessions(Node, Host, SHA);
            Error ->
                ?INFO_MSG("Error in redis clean up: ~p", [Error]),
                throw(Error)
    end.

load_script() ->
    ejabberd_redis:q(["SCRIPT", "LOAD", 
        ["redis.replicate_commands() ",
        "local cursor = redis.call('GET', KEYS[3]) or 0 ",
        "local scan_result = redis.call('HSCAN', KEYS[1], cursor, 'COUNT', ARGV[1]) ",
        "local newcursor = scan_result[1] ",
        "local cursor = redis.call('SET', KEYS[3], newcursor) ",
        "redis.call('EXPIRE', KEYS[3], ", ?DELETION_CURSOR_TIMEOUT_SEC , ") ",
        "for key,value in ipairs(scan_result[2]) do ",
            "local uskey, sidkey = string.match(value, '(.*)||(.*)') ",
            "if uskey and sidkey then ",
                "redis.call('HDEL', uskey, sidkey) ",
                "redis.call('HDEL', KEYS[1], value) ",
            "else ", 
                "redis.call('HDEL', KEYS[2], value) ",
            "end ",
        "end ",
        " return newcursor "
        ]
    ]).
