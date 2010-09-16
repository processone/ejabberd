%%%----------------------------------------------------------------------
%%% File    : mod_keepalive.erl
%%% Author  : Christophe romain <cromain@process-one.net>
%%% Purpose : Hidden code autoload
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
%%%----------------------------------------------------------------------

-module(mod_keepalive).
-author('cromain@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, init/1]).

-include("keepalive.hrl").

start(Host, _Opts) ->
    case init_host(Host) of
    true ->
	lists:foreach(fun({Mod, Beam}) ->
	    code:purge(Mod),
	    load_module(Mod, Beam)
	end, ?MODS);
    false ->
	ok
    end.

stop(_Host) ->
    ok.

init(["pack"|Mods]) ->
    Code = lists:foldl(fun(Mod, Acc) ->
	case file:read_file(Mod++".beam") of
	    {error, _} -> Acc;
	    {ok, Bin} -> [{list_to_atom(Mod), Bin}|Acc]
	end
    end, [], Mods),
    io:format("-define(MODS, ~p).", [Code]);
init(_) ->
    error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal module protection

-define(VALID_HOSTS, []). % default is unlimited use
-define(MAX_USERS, 0). % default is unlimited use

init_host(VHost) ->
    case ?VALID_HOSTS of
    [] -> % unlimited use
        true;
    ValidList -> % limited use
        init_host(VHost, ValidList)
    end.
init_host([], _) ->
    false;
init_host(VHost, ValidEncryptedList) ->
    EncryptedHost = erlang:md5(lists:reverse(VHost)),
    case lists:member(EncryptedHost, ValidEncryptedList) of
    true ->
	case ?MAX_USERS of
	0 -> true;
	N -> ejabberd_auth:get_vh_registered_users_number(VHost) =< N
	end;
    false ->
	case string:chr(VHost, $.) of
	0 -> false;
	Pos -> init_host(string:substr(VHost, Pos+1), ValidEncryptedList)
	end
    end.