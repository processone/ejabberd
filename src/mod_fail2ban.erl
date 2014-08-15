%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_fail2ban).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, c2s_auth_result/4, check_bl_c2s/2]).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, _Opts) ->
    catch ets:new(failed_auth, [named_table, public]),
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100),
    ejabberd_hooks:add(check_bl_c2s, ?MODULE, check_bl_c2s, 100).

stop(Host) ->
    ejabberd_hooks:delete(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100),
    ejabberd_hooks:delete(check_bl_c2s, ?MODULE, check_bl_c2s, 100).

%%%===================================================================
%%% Internal functions
%%%===================================================================
c2s_auth_result(false, _User, _Server, {Addr, _Port}) ->
    case ets:lookup(failed_auth, Addr) of
	[] ->
	    ets:insert(failed_auth, {Addr, 1});
	_ ->
	    ets:update_counter(failed_auth, Addr, 1)
    end,
    timer:sleep(3);
c2s_auth_result(true, _User, _Server, _AddrPort) ->
    ok.

check_bl_c2s(_Acc, Addr) ->
    case ets:lookup(failed_auth, Addr) of
	[{Addr, N}] when N >= 100 ->
	    {stop, true};
	_ ->
	    false
    end.
