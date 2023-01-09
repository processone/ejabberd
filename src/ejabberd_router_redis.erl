%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 28 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
%%%-------------------------------------------------------------------
-module(ejabberd_router_redis).
-behaviour(ejabberd_router).
-behaviour(gen_server).

%% API
-export([init/0, register_route/5, unregister_route/3, find_routes/1,
	 get_all_routes/0]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
	 terminate/2, code_change/3, start_link/0]).

-include("logger.hrl").
-include("ejabberd_router.hrl").

-record(state, {}).

-define(ROUTES_KEY, <<"ejabberd:routes">>).
-define(DOMAINS_KEY, <<"ejabberd:domains">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Spec = {?MODULE, {?MODULE, start_link, []},
	    transient, 5000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_backend_sup, Spec) of
	{ok, _Pid} -> ok;
	Err -> Err
    end.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_route(Domain, ServerHost, LocalHint, _, Pid) ->
    DomKey = domain_key(Domain),
    PidKey = term_to_binary(Pid),
    T = term_to_binary({ServerHost, LocalHint}),
    case ejabberd_redis:multi(
	   fun() ->
		   ejabberd_redis:hset(DomKey, PidKey, T),
		   ejabberd_redis:sadd(?DOMAINS_KEY, [Domain]),
		   if Domain /= ServerHost ->
			   ejabberd_redis:sadd(?ROUTES_KEY, [Domain]);
		      true ->
			   ok
		   end
	   end) of
	{ok, _} ->
	    ok;
	{error, _} ->
	    {error, db_failure}
    end.

unregister_route(Domain, _, Pid) ->
    DomKey = domain_key(Domain),
    PidKey = term_to_binary(Pid),
    try
	{ok, Num} = ejabberd_redis:hdel(DomKey, [PidKey]),
	if Num > 0 ->
		{ok, Len} = ejabberd_redis:hlen(DomKey),
		if Len == 0 ->
			{ok, _} = ejabberd_redis:multi(
				    fun() ->
					    ejabberd_redis:del([DomKey]),
					    ejabberd_redis:srem(?ROUTES_KEY, [Domain]),
					    ejabberd_redis:srem(?DOMAINS_KEY, [Domain])
				    end),
			ok;
		   true ->
			ok
		end;
	   true ->
		ok
	end
    catch _:{badmatch, {error, _}} ->
	    {error, db_failure}
    end.

find_routes(Domain) ->
    DomKey = domain_key(Domain),
    case ejabberd_redis:hgetall(DomKey) of
	{ok, Vals} ->
	    {ok, decode_routes(Domain, Vals)};
	_ ->
	    {error, db_failure}
    end.

get_all_routes() ->
    case ejabberd_redis:smembers(?ROUTES_KEY) of
	{ok, Routes} ->
	    {ok, Routes};
	_ ->
	    {error, db_failure}
    end.

get_all_domains() ->
    case ejabberd_redis:smembers(?DOMAINS_KEY) of
	{ok, Domains} ->
	    {ok, Domains};
	_ ->
	    {error, db_failure}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    clean_table(),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

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
clean_table() ->
    ?DEBUG("Cleaning Redis route entries...", []),
    lists:foreach(
      fun(#route{domain = Domain, pid = Pid}) when node(Pid) == node() ->
	      unregister_route(Domain, undefined, Pid);
	 (_) ->
	      ok
      end, find_routes()).

find_routes() ->
    case get_all_domains() of
	{ok, Domains} ->
	    lists:flatmap(
	      fun(Domain) ->
		      case find_routes(Domain) of
			  {ok, Routes} -> Routes;
			  {error, _} -> []
		      end
	      end, Domains);
	{error, _} ->
	    []
    end.

domain_key(Domain) ->
    <<"ejabberd:route:", Domain/binary>>.

decode_routes(Domain, Vals) ->
    lists:map(
      fun({Pid, Data}) ->
	      {ServerHost, LocalHint} = binary_to_term(Data),
	      #route{domain = Domain,
		     pid = binary_to_term(Pid),
		     server_host = ServerHost,
		     local_hint = LocalHint}
      end, Vals).
