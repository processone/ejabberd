%%% ====================================================================
%%% This software is copyright 2006-2010, ProcessOne.
%%%
%%% mod_autofilter
%%%
%%% @copyright 2006-2010 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================


-module(mod_autofilter).
-author('christophe.romain@process-one.net').

-behaviour(gen_mod).

% module functions
-export([start/2,stop/1,is_loaded/0]).
-export([offline_message/3,filter_packet/1,close_session/2,close_session/3]).
-export([deny/2,allow/2,denied/0,listed/0,purge/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("licence.hrl").

-record(autofilter, {key, timestamp=0, count=1, drop=false, reason}).

start(Host, Opts) ->
    case ?IS_VALID of
    true ->
	mnesia:create_table(autofilter, [
		{disc_copies, [node()]}, {type, set},
		{attributes, record_info(fields, autofilter)} ]),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message, 10),
	ejabberd_hooks:add(filter_packet, ?MODULE, filter_packet, 10),
	ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, close_session, 10),
	case gen_mod:get_opt(purge_freq, Opts, 0) of   %% purge_freq in minutes
	0 -> 
	    no_purge;
	Freq ->
	    Keep = gen_mod:get_opt(keep, Opts, 10),  %% keep in minutes
	    timer:apply_interval(Freq*60000, ?MODULE, purge, [Keep*60])
	end,
	start;
    false ->
	not_started
    end.

stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message, 10),
    ejabberd_hooks:delete(filter_packet, ?MODULE, filter_packet, 10),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, close_session, 10),
    stop.

is_loaded() ->
    ok.

purge(Keep) ->
    ?INFO_MSG("autofilter purge",[]),
    {T1, T2, _} = now(),
    Older = T1*1000000+T2-Keep,
    lists:foreach(fun(Key) ->
	mnesia:dirty_delete({autofilter, Key})
    end, mnesia:dirty_select(autofilter, [{#autofilter{key = '$1', drop = false, timestamp = '$2', _ = '_'}, [{'<', '$2', Older}], ['$1']}])),
    ok.

deny(User, Server) ->
    Key = {User, Server},
    Record = case mnesia:dirty_read({autofilter, Key}) of
    [R] -> R;
    _ -> #autofilter{key=Key}
    end,
    ?INFO_MSG("autofilter: messages from ~s@~s will be droped~n", [User, Server]),
    mnesia:dirty_write(Record#autofilter{drop=true}).

allow(User, Server) ->
    Key = {User, Server},
    ?INFO_MSG("autofilter: messages from ~s@~s are accepted~n", [User, Server]),
    mnesia:dirty_delete({autofilter, Key}).

denied() ->
    mnesia:dirty_select(autofilter, [{#autofilter{key = '$1', drop = true, reason = '$2', _ = '_'}, [], [['$1','$2']]}]).
listed() ->
    mnesia:dirty_select(autofilter, [{#autofilter{key = '$1', drop = false, reason = '$2', _ = '_'}, [], [['$1','$2']]}]).

offline_message({jid, [], _, [], [], _, []}, _To, _Packet) ->
    ok;
offline_message(From, _To, _Packet) ->
    {User, Server, _} = jlib:jid_tolower(From),
    Key = {User, Server},
    {T1, T2, _} = now(),
    T = T1*1000000+T2,
    Record = case mnesia:dirty_read({autofilter, Key}) of
    [#autofilter{timestamp=O, count=C}=R] ->
	D = T-O,
	if
	D > 3600 ->  % this is usefull only of purge_freq is not set
	    R#autofilter{timestamp=T, count=1};
	((C/D) > 1/10) and (C > 90) -> 
	    ?INFO_MSG("autofilter: messages from ~s@~s will be droped~n", [User, Server]),
	    R#autofilter{drop=true, reason=offline_flood};
	true -> 
	    R#autofilter{count=C+1}
	end;
    _ ->
	#autofilter{key=Key, timestamp=T, count=1, drop=false, reason=offline_flood}
    end,
    mnesia:dirty_write(Record),
    ok.

filter_packet({From, To, {xmlelement, "message", _, _}=Packet}) ->
    {User, Server, _} = jlib:jid_tolower(From),
    case mnesia:dirty_read({autofilter, {User, Server}}) of
    [#autofilter{drop=true}] -> drop;
    _ -> {From, To, Packet}
    end;
filter_packet(OK) ->
    OK.

close_session(SID, JID) ->
    close_session(SID, JID, []).
close_session(_SID, {jid, _, _, _, User, Server, _}, _Info) ->
    % this allows user, except for blocked ones
    lists:foreach(fun(#autofilter{key=Key}) ->
	mnesia:dirty_delete({autofilter, Key})
    end, mnesia:dirty_match_object(#autofilter{key={User, Server}, drop = false, _ = '_'})),
    ok. 
