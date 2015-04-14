%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2014-2015   ProcessOne
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

%%%-------------------------------------------------------------------
-module(mod_fail2ban).

-behaviour(gen_mod).
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, stop/1, c2s_auth_result/4, check_bl_c2s/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-define(C2S_AUTH_BAN_LIFETIME, 3600). %% 1 hour
-define(C2S_MAX_AUTH_FAILURES, 20).
-define(CLEAN_INTERVAL, timer:minutes(10)).

-record(state, {host = <<"">> :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

c2s_auth_result(false, _User, LServer, {Addr, _Port}) ->
    BanLifetime = gen_mod:get_module_opt(
		    LServer, ?MODULE, c2s_auth_ban_lifetime,
		    fun(T) when is_integer(T), T > 0 -> T end,
		    ?C2S_AUTH_BAN_LIFETIME),
    MaxFailures = gen_mod:get_module_opt(
		    LServer, ?MODULE, c2s_max_auth_failures,
		    fun(I) when is_integer(I), I > 0 -> I end,
		    ?C2S_MAX_AUTH_FAILURES),
    UnbanTS = unban_timestamp(BanLifetime),
    case ets:lookup(failed_auth, Addr) of
	[{Addr, N, _, _}] ->
	    ets:insert(failed_auth, {Addr, N+1, UnbanTS, MaxFailures});
	[] ->
	    ets:insert(failed_auth, {Addr, 1, UnbanTS, MaxFailures})
    end;
c2s_auth_result(true, _User, _Server, _AddrPort) ->
    ok.

check_bl_c2s(_Acc, Addr, Lang) ->
    case ets:lookup(failed_auth, Addr) of
	[{Addr, N, TS, MaxFailures}] when N >= MaxFailures ->
	    case TS > now() of
		true ->
		    IP = jlib:ip_to_list(Addr),
		    UnbanDate = format_date(
				    calendar:now_to_universal_time(TS)),
		    LogReason = io_lib:fwrite(
				  "Too many (~p) failed authentications "
				  "from this IP address (~s). The address "
				  "will be unblocked at ~s UTC",
				  [N, IP, UnbanDate]),
		    ReasonT = io_lib:fwrite(
				translate:translate(
				  Lang,
				  <<"Too many (~p) failed authentications "
				    "from this IP address (~s). The address "
				    "will be unblocked at ~s UTC">>),
				[N, IP, UnbanDate]),
		    {stop, {true, LogReason, ReasonT}};
		false ->
		    ets:delete(failed_auth, Addr),
		    false
	    end;
	_ ->
	    false
    end.

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    catch ets:new(failed_auth, [named_table, public]),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host, _Opts]) ->
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100),
    ejabberd_hooks:add(check_bl_c2s, ?MODULE, check_bl_c2s, 100),
    erlang:send_after(?CLEAN_INTERVAL, self(), clean),
    {ok, #state{host = Host}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    ?ERROR_MSG("got unexpected cast = ~p", [_Msg]),
    {noreply, State}.

handle_info(clean, State) ->
    ?DEBUG("cleaning ~p ETS table", [failed_auth]),
    Now = now(),
    ets:select_delete(
      failed_auth,
      ets:fun2ms(fun({_, _, UnbanTS, _}) -> UnbanTS =< Now end)),
    erlang:send_after(?CLEAN_INTERVAL, self(), clean),
    {noreply, State};
handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info = ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100),
    case is_loaded_at_other_hosts(Host) of
	true ->
	    ok;
	false ->
	    ejabberd_hooks:delete(check_bl_c2s, ?MODULE, check_bl_c2s, 100),
	    ets:delete(failed_auth)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
unban_timestamp(BanLifetime) ->
    {MegaSecs, MSecs, USecs} = now(),
    UnbanSecs = MegaSecs * 1000000 + MSecs + BanLifetime,
    {UnbanSecs div 1000000, UnbanSecs rem 1000000, USecs}.

is_loaded_at_other_hosts(Host) ->
    lists:any(
      fun(VHost) when VHost == Host ->
	      false;
	 (VHost) ->
	      gen_mod:is_loaded(VHost, ?MODULE)
      end, ?MYHOSTS).

format_date({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~2..0w:~2..0w:~2..0w ~2..0w.~2..0w.~4..0w",
		  [Hour, Minute, Second, Day, Month, Year]).
