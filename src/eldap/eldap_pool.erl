%%%-------------------------------------------------------------------
%%% File    : eldap_pool.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : LDAP connections pool
%%% Created : 12 Nov 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%% Id      : $Id$
%%%-------------------------------------------------------------------
-module(eldap_pool).
-author('xram@jabber.ru').

%% API
-export([
	 start_link/6,
	 bind/3,
	 search/2
	]).

%%====================================================================
%% API
%%====================================================================
bind(PoolName, DN, Passwd) ->
    do_request(PoolName, {bind, [DN, Passwd]}).

search(PoolName, Opts) ->
    do_request(PoolName, {search, [Opts]}).

start_link(Name, Hosts, Backups, Port, Rootdn, Passwd) ->
    PoolName = make_id(Name),
    pg2:create(PoolName),
    lists:foreach(fun(Host) ->
			  ID = erlang:ref_to_list(make_ref()),
			  case catch eldap:start_link(ID, [Host|Backups], Port, Rootdn, Passwd) of
			      {ok, Pid} ->
				  pg2:join(PoolName, Pid);
			      _ ->
				  error
			  end
		  end, Hosts).

%%====================================================================
%% Internal functions
%%====================================================================
do_request(Name, {F, Args}) ->
    case pg2:get_closest_pid(make_id(Name)) of
	Pid when is_pid(Pid) ->
	    case catch apply(eldap, F, [Pid | Args]) of
		{'EXIT', Reason} ->
		    {error, Reason};
		Reply ->
		    Reply
	    end;
	Err ->
	    Err
    end.

make_id(Name) ->
    list_to_atom("eldap_pool_" ++ Name).
