%%%-------------------------------------------------------------------
%%% File    : mod_privacy_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

-module(mod_privacy_mnesia).

-behaviour(mod_privacy).

%% API
-export([init/2, set_default/3, unset_default/2, set_lists/1,
	 set_list/4, get_lists/2, get_list/3, remove_lists/2,
	 remove_list/3, use_cache/1, import/1]).
-export([need_transform/1, transform/1]).
-export([serialize/3, deserialize_start/1, deserialize/2, serialize_privacy_record/1, deserialize_privacy_record/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_privacy.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("ejabberd_db_serialize.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, privacy,
			   [{disc_only_copies, [node()]},
			    {attributes, record_info(fields, privacy)}]).

use_cache(Host) ->
    case mnesia:table_info(privacy, storage_type) of
        disc_only_copies ->
            mod_privacy_opt:use_cache(Host);
        _ ->
            false
    end.

unset_default(LUser, LServer) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] -> ok;
		    [R] -> mnesia:write(R#privacy{default = none})
		end
	end,
    transaction(F).

set_default(LUser, LServer, Name) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			{error, notfound};
		    [#privacy{lists = Lists} = P] ->
			case lists:keymember(Name, 1, Lists) of
			    true ->
				mnesia:write(P#privacy{default = Name,
						       lists = Lists});
			    false ->
				{error, notfound}
			end
		end
	end,
    transaction(F).

remove_list(LUser, LServer, Name) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		    [] ->
			{error, notfound};
		    [#privacy{default = Default, lists = Lists} = P] ->
			if Name == Default ->
				{error, conflict};
			   true ->
				NewLists = lists:keydelete(Name, 1, Lists),
				mnesia:write(P#privacy{lists = NewLists})
			end
		end
	end,
    transaction(F).

set_lists(Privacy) ->
    mnesia:dirty_write(Privacy).

set_list(LUser, LServer, Name, List) ->
    F = fun () ->
		case mnesia:wread({privacy, {LUser, LServer}}) of
		    [] ->
			NewLists = [{Name, List}],
			mnesia:write(#privacy{us = {LUser, LServer},
					      lists = NewLists});
		    [#privacy{lists = Lists} = P] ->
			NewLists1 = lists:keydelete(Name, 1, Lists),
			NewLists = [{Name, List} | NewLists1],
			mnesia:write(P#privacy{lists = NewLists})
		end
	end,
    transaction(F).

get_list(LUser, LServer, Name) ->
    case mnesia:dirty_read(privacy, {LUser, LServer}) of
	[#privacy{default = Default, lists = Lists}] when Name == default ->
	    case lists:keyfind(Default, 1, Lists) of
		{_, List} -> {ok, {Default, List}};
		false -> error
	    end;
	[#privacy{lists = Lists}] ->
	    case lists:keyfind(Name, 1, Lists) of
		{_, List} -> {ok, {Name, List}};
		false -> error
	    end;
	[] ->
	    error
    end.

get_lists(LUser, LServer) ->
    case mnesia:dirty_read(privacy, {LUser, LServer}) of
        [#privacy{} = P] ->
            {ok, P};
        _ ->
            error
    end.

remove_lists(LUser, LServer) ->
    F = fun () -> mnesia:delete({privacy, {LUser, LServer}}) end,
    transaction(F).

import(#privacy{} = P) ->
    mnesia:dirty_write(P).

need_transform({privacy, {U, S}, _, _}) when is_list(U) orelse is_list(S) ->
    ?INFO_MSG("Mnesia table 'privacy' will be converted to binary", []),
    true;
need_transform(_) ->
    false.

transform(#privacy{us = {U, S}, default = Def, lists = Lists} = R) ->
    NewLists = lists:map(
		 fun({Name, Ls}) ->
			 NewLs = lists:map(
				   fun(#listitem{value = Val} = L) ->
					   NewVal = case Val of
                                                        {LU, LS, LR} ->
                                                            {iolist_to_binary(LU),
                                                             iolist_to_binary(LS),
                                                             iolist_to_binary(LR)};
                                                        none -> none;
                                                        both -> both;
                                                        from -> from;
                                                        to -> to;
                                                        _ -> iolist_to_binary(Val)
                                                    end,
					   L#listitem{value = NewVal}
				   end, Ls),
			 {iolist_to_binary(Name), NewLs}
		 end, Lists),
    NewDef = case Def of
		 none -> none;
		 _ -> iolist_to_binary(Def)
	     end,
    NewUS = {iolist_to_binary(U), iolist_to_binary(S)},
    R#privacy{us = NewUS, default = NewDef, lists = NewLists}.

serialize_privacy_record(#privacy{us = {U, S}, default = Def, lists = Lists}) ->
    List2 = lists:map(
        fun({Name, Rules}) ->
	    Rules2 = lists:map(
	        fun(#listitem{type = Type, value = Value, action = Action, order = Order,
	                      match_all = MA, match_iq = MI, match_message = MM,
	                      match_presence_in = MPI, match_presence_out = MPO}) ->
		    Value2 = case Type of
				jid -> <<"J:", (jid:encode(Value))/binary>>;
				group -> <<"G:", Value/binary>>;
				none -> nothing;
				V -> V
			    end,

		    {Value2, Action, Order, MA, MI, MM, MPI, MPO}
		end, Rules),
	    {Name, Rules2}
	end, Lists),
    Def2 = case Def of
	       none -> <<>>;
	       _ -> Def
	   end,
    #serialize_privacy_v1{serverhost = S, username = U, default = Def2, lists = List2}.

deserialize_privacy_record(#serialize_privacy_v1{serverhost = S, username = U, default = Def, lists = List}) ->
    List2 = lists:map(
	fun({Name, Rules}) ->
	    Rules2 = lists:map(
		fun({Value, Action, Order, MA, MI, MM, MPI, MPO}) ->
		    {Type, Value2} = case Value of
					 <<"J:", J/binary>> ->
					     {jid, jid:tolower(jid:decode(J))};
					 <<"G:", G/binary>> -> {group, G};
					 nothing -> {none, none};
					 Other -> {subscription, Other}
				     end,
		    #listitem{type = Type, value = Value2, action = Action, order = Order,
		              match_all = MA, match_iq = MI, match_message = MM,
		              match_presence_in = MPI, match_presence_out = MPO}
		end, Rules),
	    {Name, Rules2}
	end, List),
    Def2 = case Def of
	       <<>> -> none;
	       _ -> Def
	   end,
    #privacy{us = {U, S}, default = Def2, lists = List2}.

serialize(LServer, BatchSize, undefined) ->
    Conv =
	fun([]) -> skip;
	   ([#privacy{us = {_, S}} = Priv]) when S == LServer ->
	       {ok, serialize_privacy_record(Priv)};
	   (_) -> skip
	end,
    ejabberd_db_serialize:iter_records([ejabberd_db_serialize:mnesia_iter(privacy, Conv)],
				       [], BatchSize);
serialize(_LServer, BatchSize, Key) ->
    ejabberd_db_serialize:iter_records(Key, [], BatchSize).

deserialize_start(LServer) ->
    mnesia:transaction(
	fun() ->
	    Keys = mnesia:select(privacy,
	                         ets:fun2ms(
				     fun(#privacy{us = US}) when element(2, US) == LServer -> US end)),
	    lists:foreach(fun(Key) -> mnesia:delete(privacy, Key, write) end, Keys)
	end),
    ok.

deserialize(_LServer, Batch) ->
    F = fun() ->
	lists:foldl(
	    fun(_, {error, _} = Err) ->
		Err;
	       (#serialize_privacy_v1{} = Ser, _) ->
		   mnesia:write(deserialize_privacy_record(Ser))
	    end,
	    ok,
	    Batch)
	end,
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted, Reason} ->
	    {error, iolist_to_binary(io_lib:format("Error when writing privacy data: ~p", [Reason]))}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, db_failure}
    end.
