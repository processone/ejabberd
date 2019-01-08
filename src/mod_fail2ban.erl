%%%-------------------------------------------------------------------
%%% File    : mod_fail2ban.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : 
%%% Created : 15 Aug 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2019   ProcessOne
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

-module(mod_fail2ban).

-behaviour(gen_mod).
-behaviour(gen_server).

%% API
-export([start/2, stop/1, reload/3, c2s_auth_result/3,
	 c2s_stream_started/2]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, mod_options/1, depends/2]).

%% ejabberd command.
-export([get_commands_spec/0, unban/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(CLEAN_INTERVAL, timer:minutes(10)).

-record(state, {host = <<"">> :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec c2s_auth_result(ejabberd_c2s:state(), boolean(), binary())
      -> ejabberd_c2s:state() | {stop, ejabberd_c2s:state()}.
c2s_auth_result(#{ip := {Addr, _}, lserver := LServer} = State, false, _User) ->
    case is_whitelisted(LServer, Addr) of
	true ->
	    State;
	false ->
	    BanLifetime = gen_mod:get_module_opt(
			    LServer, ?MODULE, c2s_auth_ban_lifetime),
	    MaxFailures = gen_mod:get_module_opt(
			    LServer, ?MODULE, c2s_max_auth_failures),
	    UnbanTS = p1_time_compat:system_time(seconds) + BanLifetime,
	    Attempts = case ets:lookup(failed_auth, Addr) of
		[{Addr, N, _, _}] ->
			       ets:insert(failed_auth,
					  {Addr, N+1, UnbanTS, MaxFailures}),
			       N+1;
		[] ->
			       ets:insert(failed_auth,
					  {Addr, 1, UnbanTS, MaxFailures}),
			       1
	    end,
	    if Attempts >= MaxFailures ->
		    log_and_disconnect(State, Attempts, UnbanTS);
	       true ->
		    State
	    end
    end;
c2s_auth_result(#{ip := {Addr, _}} = State, true, _User) ->
    ets:delete(failed_auth, Addr),
    State.

-spec c2s_stream_started(ejabberd_c2s:state(), stream_start())
      -> ejabberd_c2s:state() | {stop, ejabberd_c2s:state()}.
c2s_stream_started(#{ip := {Addr, _}} = State, _) ->
    case ets:lookup(failed_auth, Addr) of
	[{Addr, N, TS, MaxFailures}] when N >= MaxFailures ->
	    case TS > p1_time_compat:system_time(seconds) of
		true ->
		    log_and_disconnect(State, N, TS);
		false ->
		    ets:delete(failed_auth, Addr),
		    State
	    end;
	_ ->
	    State
    end.

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    catch ets:new(failed_auth, [named_table, public,
				{heir, erlang:group_leader(), none}]),
    ejabberd_commands:register_commands(get_commands_spec()),
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_commands:unregister_commands(get_commands_spec());
        true ->
            ok
    end,
    gen_mod:stop_child(?MODULE, Host).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host, _Opts]) ->
    process_flag(trap_exit, true),
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100),
    ejabberd_hooks:add(c2s_stream_started, Host, ?MODULE, c2s_stream_started, 100),
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
    Now = p1_time_compat:system_time(seconds),
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
    ejabberd_hooks:delete(c2s_stream_started, Host, ?MODULE, c2s_stream_started, 100),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	true ->
	    ok;
	false ->
	    ets:delete(failed_auth)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd command callback.
%%--------------------------------------------------------------------
-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = unban_ip, tags = [accounts],
			desc = "Remove banned IP addresses from the fail2ban table",
			longdesc = "Accepts an IP address with a network mask. "
			    "Returns the number of unbanned addresses, or a negative integer if there were any error.",
			module = ?MODULE, function = unban,
			args = [{address, binary}],
			args_example = [<<"::FFFF:127.0.0.1/128">>],
			args_desc = ["IP address, optionally with network mask."],
			result_example = 3,
			result_desc = "Amount of unbanned entries, or negative in case of error.",
			result = {unbanned, integer}}].

-spec unban(string()) -> integer().
unban(S) ->
    case acl:parse_ip_netmask(S) of
	{ok, Net, Mask} ->
	    unban(Net, Mask);
	error ->
	    ?WARNING_MSG("Invalid network address when trying to unban: ~p", [S]),
	    -1
    end.

unban(Net, Mask) ->
    ets:foldl(
	fun({Addr, _, _, _}, Acc)  ->
	    case acl:ip_matches_mask(Addr, Net, Mask) of
		true ->
		    ets:delete(failed_auth, Addr),
		    Acc+1;
		false -> Acc
	    end
	end,
	0,
	failed_auth).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec log_and_disconnect(ejabberd_c2s:state(), pos_integer(), non_neg_integer())
      -> {stop, ejabberd_c2s:state()}.
log_and_disconnect(#{ip := {Addr, _}, lang := Lang} = State, Attempts, UnbanTS) ->
    IP = misc:ip_to_list(Addr),
    UnbanDate = format_date(
		  calendar:now_to_universal_time(seconds_to_now(UnbanTS))),
    Format = <<"Too many (~p) failed authentications "
	       "from this IP address (~s). The address "
	       "will be unblocked at ~s UTC">>,
    Args = [Attempts, IP, UnbanDate],
    ?WARNING_MSG("Connection attempt from blacklisted IP ~s: ~s",
		 [IP, io_lib:fwrite(Format, Args)]),
    Err = xmpp:serr_policy_violation({Format, Args}, Lang),
    {stop, ejabberd_c2s:send(State, Err)}.

is_whitelisted(Host, Addr) ->
    Access = gen_mod:get_module_opt(Host, ?MODULE, access),
    acl:match_rule(Host, Access, Addr) == allow.

seconds_to_now(Secs) ->
    {Secs div 1000000, Secs rem 1000000, 0}.

format_date({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~2..0w:~2..0w:~2..0w ~2..0w.~2..0w.~4..0w",
		  [Hour, Minute, Second, Day, Month, Year]).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(c2s_auth_ban_lifetime) ->
    fun (T) when is_integer(T), T > 0 -> T end;
mod_opt_type(c2s_max_auth_failures) ->
    fun (I) when is_integer(I), I > 0 -> I end.

mod_options(_Host) ->
    [{access, none},
     {c2s_auth_ban_lifetime, 3600}, %% one hour
     {c2s_max_auth_failures, 20}].
