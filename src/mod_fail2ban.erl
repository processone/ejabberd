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
-export([start/2, stop/1, c2s_auth_result/4]).

-include("jlib.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(Host, _Opts) ->
    ets:new(failed_auth, [bag, named_table, public]),
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100).

stop(Host) ->
    ejabberd_hooks:delete(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 100).

%%%===================================================================
%%% Internal functions
%%%===================================================================
c2s_auth_result(true, User, Server, {Addr, _Port}) ->
    case jlib:make_jid(User, Server, <<"">>) of
	#jid{luser = LUser, lserver = LServer} ->
	    US = {LUser, LServer},
	    Objs = ets:lookup(failed_auth, Addr),
	    case lists:filter(fun({_, US1, _}) -> US1 == US end, Objs) of
		[_|_] ->
		    ets:match_delete(failed_auth, {'_', US, '_'});
		[] ->
		    true
	    end;
	_ ->
	    false
    end;
c2s_auth_result(false, User, Server, {Addr, _Port}) ->
    case jlib:make_jid(User, Server, <<"">>) of
	#jid{luser = LUser, lserver = LServer} ->
	    US = {LUser, LServer},
	    ets:insert(failed_auth, {Addr, US, now()}),
	    Objs = ets:match_object(failed_auth, {'_', US, '_'}),
	    Timeout = round(math:exp(length(Objs))),
	    timer:sleep(timer:seconds(Timeout));
	_ ->
	    ok
    end.
