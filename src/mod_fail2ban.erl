%%%-------------------------------------------------------------------
%%% File    : mod_fail2ban.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose :
%%% Created : 15 Aug 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2025   ProcessOne
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
	 mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

%% ejabberd command.
-export([get_commands_spec/0, unban/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-define(CLEAN_INTERVAL, timer:minutes(10)).

-record(state, {host = <<"">> :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec c2s_auth_result(ejabberd_c2s:state(), true | {false, binary()}, binary())
      -> ejabberd_c2s:state() | {stop, ejabberd_c2s:state()}.
c2s_auth_result(#{sasl_mech := Mech} = State, {false, _}, _User)
  when Mech == <<"EXTERNAL">> ->
    State;
c2s_auth_result(#{ip := {Addr, _}, lserver := LServer} = State, {false, _}, _User) ->
    case is_whitelisted(LServer, Addr) of
	true ->
	    State;
	false ->
	    BanLifetime = mod_fail2ban_opt:c2s_auth_ban_lifetime(LServer),
	    MaxFailures = mod_fail2ban_opt:c2s_max_auth_failures(LServer),
	    UnbanTS = current_time() + BanLifetime,
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
	    case TS > current_time() of
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
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()),
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
init([Host|_]) ->
    process_flag(trap_exit, true),
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100),
    ejabberd_hooks:add(c2s_stream_started, Host, ?MODULE, c2s_stream_started, 100),
    erlang:send_after(?CLEAN_INTERVAL, self(), clean),
    {ok, #state{host = Host}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast = ~p", [Msg]),
    {noreply, State}.

handle_info(clean, State) ->
    ?DEBUG("Cleaning ~p ETS table", [failed_auth]),
    Now = current_time(),
    ets:select_delete(
      failed_auth,
      ets:fun2ms(fun({_, _, UnbanTS, _}) -> UnbanTS =< Now end)),
    erlang:send_after(?CLEAN_INTERVAL, self(), clean),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info = ~p", [Info]),
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

-spec unban(binary()) -> integer().
unban(S) ->
    case misc:parse_ip_mask(S) of
	{ok, {Net, Mask}} ->
	    unban(Net, Mask);
	error ->
	    ?WARNING_MSG("Invalid network address when trying to unban: ~p", [S]),
	    -1
    end.

-spec unban(inet:ip_address(), 0..128) -> non_neg_integer().
unban(Net, Mask) ->
    ets:foldl(
	fun({Addr, _, _, _}, Acc)  ->
	    case misc:match_ip_mask(Addr, Net, Mask) of
		true ->
		    ets:delete(failed_auth, Addr),
		    Acc+1;
		false -> Acc
	    end
	end, 0, failed_auth).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec log_and_disconnect(ejabberd_c2s:state(), pos_integer(), non_neg_integer())
      -> {stop, ejabberd_c2s:state()}.
log_and_disconnect(#{ip := {Addr, _}, lang := Lang} = State, Attempts, UnbanTS) ->
    IP = misc:ip_to_list(Addr),
    UnbanDate = format_date(
		  calendar:now_to_universal_time(msec_to_now(UnbanTS))),
    Format = ?T("Too many (~p) failed authentications "
		"from this IP address (~s). The address "
		"will be unblocked at ~s UTC"),
    Args = [Attempts, IP, UnbanDate],
    ?WARNING_MSG("Connection attempt from blacklisted IP ~ts: ~ts",
		 [IP, io_lib:fwrite(Format, Args)]),
    Err = xmpp:serr_policy_violation({Format, Args}, Lang),
    {stop, ejabberd_c2s:send(State, Err)}.

-spec is_whitelisted(binary(), inet:ip_address()) -> boolean().
is_whitelisted(Host, Addr) ->
    Access = mod_fail2ban_opt:access(Host),
    acl:match_rule(Host, Access, Addr) == allow.

-spec msec_to_now(pos_integer()) -> erlang:timestamp().
msec_to_now(MSecs) ->
    Secs = MSecs div 1000,
    {Secs div 1000000, Secs rem 1000000, 0}.

-spec format_date(calendar:datetime()) -> iolist().
format_date({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~2..0w:~2..0w:~2..0w ~2..0w.~2..0w.~4..0w",
		  [Hour, Minute, Second, Day, Month, Year]).

current_time() ->
    erlang:system_time(millisecond).

mod_opt_type(access) ->
    econf:acl();
mod_opt_type(c2s_auth_ban_lifetime) ->
    econf:timeout(second);
mod_opt_type(c2s_max_auth_failures) ->
    econf:pos_int().

mod_options(_Host) ->
    [{access, none},
     {c2s_auth_ban_lifetime, timer:hours(1)},
     {c2s_max_auth_failures, 20}].

mod_doc() ->
    #{desc =>
          [?T("The module bans IPs that show the malicious signs. "
              "Currently only C2S authentication failures are detected."), "",
           ?T("Unlike the standalone program, 'mod_fail2ban' clears the "
              "record of authentication failures after some time since the "
              "first failure or on a successful authentication. "
              "It also does not simply block network traffic, but "
              "provides the client with a descriptive error message."), "",
	   ?T("WARNING: You should not use this module behind a proxy or load "
	      "balancer. ejabberd will see the failures as coming from the "
	      "load balancer and, when the threshold of auth failures is "
	      "reached, will reject all connections coming from the load "
	      "balancer. You can lock all your user base out of ejabberd "
	      "when using this module behind a proxy.")],
      opts =>
          [{access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("Specify an access rule for whitelisting IP "
                     "addresses or networks. If the rule returns 'allow' "
                     "for a given IP address, that address will never be "
                     "banned. The 'AccessName' should be of type 'ip'. "
                     "The default value is 'none'.")}},
           {c2s_auth_ban_lifetime,
            #{value => "timeout()",
              desc =>
                  ?T("The lifetime of the IP ban caused by too many "
                     "C2S authentication failures. The default value is "
                     "'1' hour.")}},
           {c2s_max_auth_failures,
            #{value => ?T("Number"),
              desc =>
                  ?T("The number of C2S authentication failures to "
                     "trigger the IP ban. The default value is '20'.")}}]}.
